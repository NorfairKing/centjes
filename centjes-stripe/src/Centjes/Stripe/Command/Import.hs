{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Stripe.Command.Import
  ( runCentjesStripeImport,
    ImportError (..),
    transactionMonth,
    monthsAlreadyImported,
    monthsToImport,
    transactionsToAdd,
    untilEvidenceMissing,
  )
where

import Centjes.Compile
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format (formatModule)
import Centjes.Load
import Centjes.Location
import Centjes.Merge (mergeTransactionDeclarations)
import Centjes.Module
import Centjes.Stripe.API
import Centjes.Stripe.Aggregate
import Centjes.Stripe.Currency
import Centjes.Stripe.Declarations
import Centjes.Stripe.OptParse
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Conduit
import Control.Monad (when, (<=<))
import Control.Monad.Logger
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Calendar.Month
import Error.Diagnose
import qualified Money.Account as Account
import Money.QuantisationFactor (QuantisationFactor)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO

-- | Everything this importer refuses to do, rather than guess at.
--
-- Rendered the way every other centjes error is, through 'ToReport', so that a run that
-- cannot continue says why in the same shape as a ledger that does not check.
data ImportError
  = -- | The output file, and the ledger directory it is not inside.
    ImportErrorOutputOutsideLedger !(Path Abs File) !(Path Abs Dir)
  | -- | The output file the ledger does not import, and the ledger.
    ImportErrorOutputNotImported !(Path Rel File) !(Path Abs File)
  | ImportErrorNoSuchCurrency !CurrencySymbol
  | ImportErrorApi !StripeApiError
  | -- | Which report, and what was wrong with the CSV.
    ImportErrorReport !ReportTypeId !ReportError
  | ImportErrorAggregate !StripeError
  | ImportErrorDeclaration !DeclarationError

instance ToReport ImportError where
  toReport = \case
    ImportErrorOutputOutsideLedger output ledgerDirectory ->
      Err
        (Just "SI_OUTPUT_OUTSIDE_LEDGER")
        ( unwords
            [ "The output file",
              fromAbsFile output,
              "is not inside the ledger's directory",
              fromAbsDir ledgerDirectory <> "."
            ]
        )
        []
        [Hint "The ledger cannot import a file outside its own directory, and this importer has to read back what it wrote."]
    ImportErrorOutputNotImported output ledgerFile ->
      Err
        (Just "SI_OUTPUT_NOT_IMPORTED")
        (unwords ["The ledger does not import", fromRelFile output <> ",", "but that file exists."])
        []
        [ Hint "This importer reads back what it has already written and never rewrites a month, which it cannot do for a file the ledger does not include.",
          Hint (unwords ["Add an import of it to", fromAbsFile ledgerFile <> "."])
        ]
    ImportErrorNoSuchCurrency currency ->
      Err
        (Just "SI_NO_SUCH_CURRENCY")
        (unwords ["This importer is set to read", Text.unpack (CurrencySymbol.toText currency), "but the ledger declares no such currency."])
        []
        [Hint "Add a currency declaration for it."]
    ImportErrorApi apiError ->
      Err (Just "SI_API") (Text.unpack (renderStripeApiError apiError)) [] []
    ImportErrorReport reportTypeId' reportError ->
      Err
        (Just "SI_REPORT")
        (unwords ["The", show (unReportTypeId reportTypeId'), "report could not be read:"])
        []
        [Hint (renderReportError reportError)]
    ImportErrorAggregate stripeError -> toReport stripeError
    ImportErrorDeclaration declarationError -> toReport declarationError

-- | What the importer runs in: one error ends the run, and it is reported rather than
-- printed bare.
type ImportM = ValidationT ImportError (LoggingT IO)

runCentjesStripeImport :: Settings -> ImportSettings -> IO ()
runCentjesStripeImport settings importSettings = runStderrLoggingT $ do
  (declarations, fileMap) <- loadModules' (settingLedgerFile settings)
  let diag = diagFromFileMap fileMap
  currencies <- liftIO $ checkValidation diag $ compileDeclarationsCurrencies declarations
  validation <- runValidationT (importStripe settings importSettings currencies fileMap)
  liftIO $ checkValidation diag validation

importStripe ::
  Settings ->
  ImportSettings ->
  Map CurrencySymbol (GenLocated SourceSpan QuantisationFactor) ->
  Map (Path Rel File) (Text, LModule) ->
  ImportM ()
importStripe Settings {..} importSettings@ImportSettings {..} currencies fileMap = do
  let ledgerDirectory = parent settingLedgerFile
  outputRelFile <- case stripProperPrefix ledgerDirectory importSettingOutput of
    Nothing -> validationTFailure $ ImportErrorOutputOutsideLedger importSettingOutput ledgerDirectory
    Just relFile -> pure relFile

  -- What the output file already says, from the parse of that file itself rather than
  -- sifted out of the whole ledger's declarations by source position.  Everything this
  -- importer promises rests on reading this back in full: a declaration it fails to see
  -- here is a declaration the rewrite drops, and a month it fails to see is a month it
  -- writes again over whatever the file had turned that month into.
  existingModule <- case M.lookup outputRelFile fileMap of
    Just (_, parsedModule) -> pure (stripModuleAnnotation parsedModule)
    Nothing -> do
      -- Not part of the ledger.  Creating it is fine; finding one already there is not,
      -- because then this importer cannot read back what it wrote and would replace
      -- by-hand edits and already-filed months with a fresh draft.
      outputExists <- doesFileExist importSettingOutput
      if outputExists
        then validationTFailure $ ImportErrorOutputNotImported outputRelFile settingLedgerFile
        else pure Module {moduleImports = [], moduleDeclarations = []}
  let alreadyThere = monthsAlreadyImported (moduleDeclarations existingModule)

  quantisationFactor <- case M.lookup importSettingCurrency currencies of
    Nothing -> validationTFailure $ ImportErrorNoSuchCurrency importSettingCurrency
    Just (Located _ quantisationFactor) -> pure quantisationFactor
  let stripeCurrency = StripeCurrency (Text.toLower (CurrencySymbol.toText importSettingCurrency))

  man <- liftIO HTTP.newTlsManager

  -- Which months are worth asking about at all: over, not already in the file, and
  -- reported on completely enough by every report this needs.
  today <- liftIO $ utctDay <$> getCurrentTime
  reportTypes <- traverse (api . fetchReportType man importSettingKey . reportKindTypeId) allReportKinds
  let (wanted, held) = monthsToImport importSettingBegin today alreadyThere reportTypes
  mapM_ (uncurry logNotYet) held

  -- One pipeline from the months worth asking about to the module to write.  A stage only
  -- runs when the next one asks it to, which is what makes 'untilEvidenceMissing' below
  -- stop the months after a held-back one from being fetched at all.
  merged <-
    runConduit $
      yieldMany wanted
        .| C.mapM (monthReports man importSettingKey stripeCurrency ledgerDirectory importSettingReportsDirectory)
        .| C.mapM
          ( liftValidation
              . mapValidationFailure ImportErrorAggregate
              . aggregateMonth
                AggregateSettings
                  { aggregateSettingHomeCountry = Text.toUpper importSettingHomeCountry,
                    aggregateSettingVATRates = swissVATRates,
                    aggregateSettingCurrency = stripeCurrency,
                    aggregateSettingFeesVATRate = importSettingFeesVATRatePercentage / 100
                  }
                quantisationFactor
          )
        .| C.mapM (evidenceFor importSettings ledgerDirectory)
        .| untilEvidenceMissing
        .| C.iterM warnAbout
        .| sinkModule importSettings quantisationFactor alreadyThere existingModule

  -- Only the bytes that differ get written.  This is the file the user owns, and a run
  -- with nothing to add has no business touching it.
  let output = formatModule merged
  case M.lookup outputRelFile fileMap of
    Just (onDisk, _) | onDisk == output -> logInfoN $ Text.pack $ unwords ["Leaving", fromRelFile outputRelFile, "alone."]
    _ -> liftIO $ SB.writeFile (fromAbsFile importSettingOutput) (TE.encodeUtf8 output)

-- | Turn the months that are being written into the module to write.
--
-- The transactions of every month at once, because the opening balance transaction comes
-- from the first month written and nothing before it is in the file.
sinkModule ::
  ImportSettings ->
  QuantisationFactor ->
  Set Month ->
  Module () ->
  ConduitT StripeMonth Void ImportM (Module ())
sinkModule importSettings quantisationFactor alreadyThere existingModule = do
  months <- C.sinkList
  transactions <-
    lift $
      liftValidation $
        mapValidationFailure ImportErrorDeclaration $
          stripeTransactions
            (importDeclarationSettings importSettings)
            quantisationFactor
            (importSettingCurrency importSettings)
            months
  let toAdd = transactionsToAdd alreadyThere transactions
  lift $
    mapM_
      ( \t ->
          logInfoN $
            Text.pack $
              unwords ["Adding", show (transactionMonth t) <> ":", maybe "no description" show (descriptionOf t)]
      )
      toAdd
  pure $ mergeTransactionDeclarations existingModule (map noLoc toAdd)

-- | Where a month's tax invoice should be, if it needs one and has not got one.
--
-- Only a month with fees needs it at all, because only a fees transaction attaches it.
evidenceFor ::
  ImportSettings ->
  Path Abs Dir ->
  StripeMonth ->
  ImportM (StripeMonth, Maybe (Path Rel File))
evidenceFor ImportSettings {..} ledgerDirectory month
  | null (stripeMonthFees month) = pure (month, Nothing)
  | otherwise = do
      taxInvoice <-
        liftValidation $
          mapValidationFailure ImportErrorDeclaration $
            monthDocumentPath
              importSettingDocumentsDirectory
              importSettingFeesAttachmentPattern
              (stripeMonthMonth month)
      exists <- doesFileExist (ledgerDirectory </> taxInvoice)
      pure (month, if exists then Nothing else Just taxInvoice)

-- | Pass months on until one has evidence missing, and stop there.
--
-- Stopping rather than skipping is what the balance assertions need: each month asserts
-- the balance Stripe closed it on, which only holds if every month between the opening
-- balance and it was booked.  A month left out would leave the next assertion short by
-- whatever moved in the gap, and since the importer never rewrites a month, the gap could
-- then only ever be filled in by hand.
--
-- A conduit only pulls what is asked for, so stopping here also means the months after a
-- held-back one are never fetched.  Nothing is lost: save the document and the next run
-- writes the whole run.
untilEvidenceMissing ::
  (MonadLogger m) =>
  ConduitT (StripeMonth, Maybe (Path Rel File)) StripeMonth m ()
untilEvidenceMissing =
  await >>= \case
    Nothing -> pure ()
    Just (month, Nothing) -> do
      yield month
      untilEvidenceMissing
    Just (month, Just document) ->
      lift $
        logNotYet (stripeMonthMonth month) $
          unwords
            [ "save Stripe's tax invoice for it at",
              fromRelFile document,
              "first, and nothing after it is imported either, because skipping a month",
              "would leave the next one's balance assertion short.",
              "The invoice is in the Stripe dashboard under Settings, Reporting and documents."
            ]

-- | The one thing about a month that nothing here can check.
--
-- Stripe's tax report only has rows for jurisdictions the account is registered in, so
-- revenue from anywhere else is absent from it and lands in foreign revenue by
-- construction.  The month's total is right either way, so the reconciliation says
-- nothing.  Saying how much is in that position is the most that can honestly be done.
warnAbout :: StripeMonth -> ImportM ()
warnAbout month = do
  when (stripeMonthRevenueWithNoTaxRow month /= Account.zero) $
    logWarnN $
      Text.pack $
        unwords
          [ show (stripeMonthMonth month),
            "has",
            show (Account.toMinimalQuantisations (stripeMonthRevenueWithNoTaxRow month)),
            "in minor units of revenue that no row of the tax report mentions at all,",
            "which is booked as foreign.  That is what Stripe reports for a country it is",
            "not registered to collect tax in.  If any of it was in fact domestic, its VAT",
            "is not being declared, and nothing here can tell."
          ]
  -- Booking a refund as an expense gets the VAT payable right but leaves turnover gross
  -- where a return wants it net of credits.
  when (any ((/= Account.zero) . revenueAtRateRefundedNet) (stripeMonthDomesticRevenues month)) $
    logWarnN $
      Text.pack $
        unwords
          [ show (stripeMonthMonth month),
            "had refunds, which are booked as an expense rather than as reduced revenue.",
            "The VAT payable is right, but turnover and input tax are both overstated by the refund,",
            "so check that month before filing."
          ]

logNotYet :: (MonadLogger m) => Month -> String -> m ()
logNotYet month why = logInfoN $ Text.pack $ unwords ["Not importing", show month, "yet:", why]

api :: StripeM a -> ImportM a
api = either (validationTFailure . ImportErrorApi) pure <=< lift . runExceptT

-- | Which months to import, and why each of the others is being held back.
--
-- A month is imported when it has ended, when the file has no transaction in it yet,
-- and when every report needed has finished being computed through the end of it.  That
-- last one is Stripe's own answer rather than a guess from today's date: fee data lags
-- days behind the rest, so a month can be over and still not reported on.
monthsToImport ::
  -- | The first month to consider.
  Month ->
  -- | Today.
  Day ->
  -- | Months the file already has.
  Set Month ->
  -- | Every report the importer needs.
  [ReportType] ->
  ([Month], [(Month, String)])
monthsToImport begin today alreadyThere reportTypes =
  let currentMonth = dayPeriod today
      candidates = [begin .. pred currentMonth]
      decide month
        | month `S.member` alreadyThere =
            Right (month, "the file already has it, and this importer never rewrites a month")
        | otherwise = case filter (not . reportTypeCoversThrough (endOf month)) reportTypes of
            [] -> Left month
            (lagging : _) ->
              Right
                ( month,
                  unwords
                    [ "Stripe has not finished computing",
                      show (unReportTypeId (reportTypeId lagging)),
                      "for it yet"
                    ]
                )
      endOf month = stripeTimestampAtMidnight (periodFirstDay (succ month))
   in partitionEithers (map decide candidates)

-- | A month's reports, saved as the evidence for it on the way through.
--
-- A report already on disk is /read/ rather than fetched again, which is what makes the
-- attached file and the booked figures the same bytes.  Fetching afresh while keeping the
-- older file would leave the ledger pointing at evidence that no longer says what the
-- ledger says: Stripe restates a report as late data arrives, and a month may already have
-- been filed from the earlier one.  Reading it back also means a month held back for its
-- tax invoice costs no report runs the second time around.
--
-- The bytes are saved only once they have parsed, so a garbled response never becomes a
-- month's evidence.
{-# ANN monthReports ("NOCOVER" :: String) #-}
monthReports ::
  Manager ->
  StripeKey ->
  StripeCurrency ->
  Path Abs Dir ->
  Path Rel Dir ->
  Month ->
  ImportM MonthReports
monthReports man key stripeCurrency ledgerDirectory reportsDirectory month = do
  let start = stripeTimestampAtMidnight (periodFirstDay month)
  let end = stripeTimestampAtMidnight (periodFirstDay (succ month))
  let currencyParameter = [("currency", TE.encodeUtf8 (unStripeCurrency stripeCurrency))]
  -- The tax report takes no currency filter, which is why every row's currency is
  -- checked when the report is read.
  let currencyParameterFor = \case
        ReportKindTax -> []
        ReportKindActivity -> currencyParameter
        ReportKindFees -> currencyParameter
        ReportKindBalance -> currencyParameter
  let table kind = do
        relFile <-
          liftValidation $
            mapValidationFailure ImportErrorDeclaration $
              monthDocumentPath reportsDirectory (reportKindAttachmentPattern kind) month
        let absFile = ledgerDirectory </> relFile
        saved <- doesFileExist absFile
        contents <-
          if saved
            then do
              logInfoN $
                Text.pack $
                  unwords
                    [ "Reading",
                      fromRelFile relFile <> ",",
                      "which is the copy the ledger refers to."
                    ]
              liftIO $ LB.readFile (fromAbsFile absFile)
            else
              api $
                fetchReport
                  man
                  key
                  (reportKindTypeId kind)
                  start
                  end
                  (reportKindColumns kind)
                  (currencyParameterFor kind)
        parsed <- case parseReportTable contents of
          Left reportError -> validationTFailure $ ImportErrorReport (reportKindTypeId kind) reportError
          Right parsed -> pure parsed
        when (not saved) $ do
          ensureDir (parent absFile)
          liftIO $ LB.writeFile (fromAbsFile absFile) contents
          logInfoN $ Text.pack $ unwords ["Saved", fromRelFile relFile <> "."]
        pure parsed
  MonthReports month
    <$> table ReportKindTax
    <*> table ReportKindActivity
    <*> table ReportKindFees
    <*> table ReportKindBalance
    <*> api (fetchPayouts man key start end)

-- | The month a transaction is dated in.
transactionMonth :: Transaction () -> Month
transactionMonth = dayPeriod . Timestamp.toDay . locatedValue . transactionTimestamp

-- | The months the output file already has a transaction in.
--
-- Keyed by the month a transaction is dated in rather than by anything written in its
-- description, so that renaming a transaction does not make the importer add its month
-- a second time.
monthsAlreadyImported :: [GenLocated () (Declaration ())] -> Set Month
monthsAlreadyImported existingDeclarations =
  S.fromList
    [ transactionMonth t
    | Located _ t <- declarationsTransactions (splitDeclarations existingDeclarations)
    ]

descriptionOf :: Transaction () -> Maybe Description
descriptionOf t = locatedValue . commentedValue <$> transactionDescription t

-- | Which of the transactions this importer computed actually get written.
--
-- The file is the source of truth.  Stripe is only where a first draft of a month comes
-- from; what the user has turned that month into is what that month is.  So a month the
-- file already has any transaction in is left exactly as it stands, and nothing computed
-- for that month is written, not even to correct it.
transactionsToAdd :: Set Month -> [Transaction ()] -> [Transaction ()]
transactionsToAdd alreadyThere = filter ((`S.notMember` alreadyThere) . transactionMonth)
