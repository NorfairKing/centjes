{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Turning what happened at Wise into transactions.
--
-- Everything here is a pure function over data a test can hold, so that what
-- ends up in a ledger is decided somewhere a test can reach.
module Centjes.Import.Wise.Declarations
  ( DeclarationSettings (..),
    ImportError (..),
    wiseTransactions,
    eventTransaction,
    RowAmounts (..),
    rowAmounts,
    conversionPrice,
    eventDescription,
  )
where

import Centjes.Description as Description
import Centjes.Import.Wise.Pair
import Centjes.Import.Wise.Statement
import Centjes.Location
import Centjes.Module
import Centjes.Validation
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose
import qualified Money.Account as Account
import qualified Money.Account as Money (Account (..))
import Money.QuantisationFactor
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- | The accounts and tags a statement is booked into.
data DeclarationSettings = DeclarationSettings
  { -- | One account for every balance.
    --
    -- A Wise account holds several currencies at once and so does a centjes
    -- account, so there is nothing to gain by splitting them, and a conversion
    -- would otherwise have to move between two accounts that are really one.
    declarationSettingAssetsAccountName :: !AccountName,
    declarationSettingExpensesAccountName :: !AccountName,
    declarationSettingIncomeAccountName :: !AccountName,
    declarationSettingFeesAccountName :: !AccountName,
    declarationSettingFeeTags :: ![Tag]
  }

-- | Something in a statement that cannot be written down as a transaction.
--
-- These name the row by its Wise id rather than by a line number: rows from
-- several statements are read into one list, so a line number would have to say
-- which file it was in as well, and the id is what you would search the
-- statement for anyway.
data ImportError
  = ImportErrorUnknownCurrency !Text !CurrencySymbol
  | ImportErrorInvalidAccount !Text !QuantisationFactor !DecimalLiteral
  | ImportErrorInvalidLiteral !Text !QuantisationFactor !Money.Account
  | ImportErrorAdd !Text !Money.Account !Money.Account
  | -- | A conversion of nothing into something, which has no rate to write.
    ImportErrorConversionWithoutAmount !Text

instance ToReport ImportError where
  toReport e =
    let report code message = Err (Just code) message [] []
     in case e of
          ImportErrorUnknownCurrency transferId symbol ->
            report
              "IE_WISE_UNKNOWN_CURRENCY"
              ( unwords
                  [ "Unknown currency:",
                    show (currencySymbolText symbol),
                    "in row",
                    show (T.unpack transferId) <> ".",
                    "Declare it in the ledger to import this statement."
                  ]
              )
          ImportErrorInvalidAccount transferId qf dl ->
            report
              "IE_WISE_INVALID_AMOUNT"
              ( unwords
                  [ "Invalid amount:",
                    show (DecimalLiteral.toString dl),
                    "with quantisation factor",
                    show (unQuantisationFactor qf),
                    "in row",
                    show (T.unpack transferId)
                  ]
              )
          ImportErrorInvalidLiteral transferId qf a ->
            report
              "IE_WISE_INVALID_LITERAL"
              ( unwords
                  [ "Invalid literal:",
                    show a,
                    "with quantisation factor",
                    show (unQuantisationFactor qf),
                    "in row",
                    show (T.unpack transferId)
                  ]
              )
          ImportErrorAdd transferId a1 a2 ->
            report
              "IE_WISE_ADD"
              ( unwords
                  [ "Could not add",
                    show a2,
                    "to",
                    show a1,
                    "in row",
                    show (T.unpack transferId)
                  ]
              )
          ImportErrorConversionWithoutAmount transferId ->
            report
              "IE_WISE_CONVERSION_WITHOUT_AMOUNT"
              ( unwords
                  [ "The conversion",
                    show (T.unpack transferId),
                    "converted nothing, so there is no rate to write down."
                  ]
              )

wiseTransactions ::
  DeclarationSettings ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  [WiseEvent] ->
  Validation ImportError [Transaction ()]
wiseTransactions settings currencies = traverse (eventTransaction settings currencies)

eventTransaction ::
  DeclarationSettings ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  WiseEvent ->
  Validation ImportError (Transaction ())
eventTransaction settings currencies event = do
  (postings, assertions, feesWereCharged) <- case event of
    WiseMovement row -> do
      amounts <- rowAmounts currencies row
      postings <- movementPostings settings row amounts
      assertions <- rowAssertion settings currencies row
      pure (postings, assertions, rowAmountsFee amounts /= Account.zero)
    WiseConversion out back -> do
      outAmounts <- rowAmounts currencies out
      backAmounts <- rowAmounts currencies back
      postings <- conversionPostings settings out outAmounts back backAmounts
      outAssertion <- rowAssertion settings currencies out
      backAssertion <- rowAssertion settings currencies back
      pure
        ( postings,
          outAssertion ++ backAssertion,
          rowAmountsFee outAmounts /= Account.zero || rowAmountsFee backAmounts /= Account.zero
        )

  let tags =
        [ noCommentsOn $ TransactionTag $ noLoc $ ExtraTag $ noLoc tag
        | feesWereCharged,
          tag <- declarationSettingFeeTags settings
        ]

  pure
    Transaction
      { transactionTimestamp = noLoc (TimestampDay (wiseEventDay event)),
        transactionDescription = noCommentsOn <$> eventDescription event,
        transactionPostings = postings,
        transactionExtras = assertions ++ tags
      }

-- | What a row did to the balance it is in.
data RowAmounts = RowAmounts
  { rowAmountsQuantisationFactor :: !QuantisationFactor,
    -- | What the balance moved by, fee included.
    rowAmountsMoved :: !Money.Account,
    -- | What Wise kept, always positive.
    rowAmountsFee :: !Money.Account,
    -- | What was actually transacted, which is the movement without the fee.
    rowAmountsTransacted :: !Money.Account
  }
  deriving (Show, Eq)

-- | Read a row's amount column as money.
--
-- The amount column is the whole movement, fee included, so what was actually
-- transacted is the movement with the fee taken back out.  This is the half of
-- that convention a test cannot check on its own: the other half is the running
-- balance assertion, which fails in @centjes check@ if this is backwards.
rowAmounts ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Row ->
  Validation ImportError RowAmounts
rowAmounts currencies row = do
  let transferId = rowId row
  rowAmountsQuantisationFactor <- quantisationFactorFor currencies transferId (rowCurrency row)
  rowAmountsMoved <- accountFromLiteral transferId rowAmountsQuantisationFactor (rowAmount row)
  rowAmountsFee <-
    maybe
      (pure Account.zero)
      (accountFromLiteral transferId rowAmountsQuantisationFactor)
      (rowTotalFees row)
  rowAmountsTransacted <- addAccounts transferId rowAmountsMoved rowAmountsFee
  pure RowAmounts {..}

-- | Money entering or leaving the account: the balance, what it came from or
-- went to, and the fee if there was one.
movementPostings ::
  DeclarationSettings ->
  Row ->
  RowAmounts ->
  Validation ImportError [Commented () (Posting ())]
movementPostings settings row RowAmounts {..} = do
  let transferId = rowId row
  let currency = rowCurrency row
  -- Negative is money that came in, which is income; positive is money that
  -- went out, which is an expense.
  let counterAccount = Account.negate rowAmountsTransacted
  let counterAccountName =
        if counterAccount < Account.zero
          then declarationSettingIncomeAccountName settings
          else declarationSettingExpensesAccountName settings
  assetsPosting <-
    posting
      transferId
      (declarationSettingAssetsAccountName settings)
      rowAmountsQuantisationFactor
      currency
      rowAmountsMoved
      Nothing
  counterPosting <-
    posting
      transferId
      counterAccountName
      rowAmountsQuantisationFactor
      currency
      counterAccount
      Nothing
  -- The balance moved by the whole amount, fee and all, so the fee needs only
  -- the expense side here: the balance side of it is already in the posting
  -- above.  A conversion is the other way around, because there the balance is
  -- split into the part that was converted and the part that was the fee.
  fee <-
    feeExpensePosting
      settings
      transferId
      rowAmountsQuantisationFactor
      currency
      rowAmountsFee
  pure (assetsPosting : counterPosting : fee)

-- | Money moving between two of your own balances, which is one transaction
-- with a currency on each side.
conversionPostings ::
  DeclarationSettings ->
  Row ->
  RowAmounts ->
  Row ->
  RowAmounts ->
  Validation ImportError [Commented () (Posting ())]
conversionPostings settings out outAmounts back backAmounts = do
  let transferId = rowId out
  price <-
    case conversionPrice
      (rowCurrency back)
      (rowAmountsTransacted outAmounts)
      (rowAmountsTransacted backAmounts) of
      Nothing -> validationFailure $ ImportErrorConversionWithoutAmount transferId
      Just p -> pure p
  outPosting <-
    posting
      transferId
      (declarationSettingAssetsAccountName settings)
      (rowAmountsQuantisationFactor outAmounts)
      (rowCurrency out)
      (rowAmountsTransacted outAmounts)
      (Just (noLoc price))
  backPosting <-
    posting
      transferId
      (declarationSettingAssetsAccountName settings)
      (rowAmountsQuantisationFactor backAmounts)
      (rowCurrency back)
      (rowAmountsTransacted backAmounts)
      Nothing
  outFees <-
    feePostings
      settings
      transferId
      (rowAmountsQuantisationFactor outAmounts)
      (rowCurrency out)
      (rowAmountsFee outAmounts)
  backFees <-
    feePostings
      settings
      transferId
      (rowAmountsQuantisationFactor backAmounts)
      (rowCurrency back)
      (rowAmountsFee backAmounts)
  pure (concat [[outPosting, backPosting], outFees, backFees])

-- | The pair of postings that books a fee: off the balance, onto the expense.
--
-- Both sides are written out because the postings that make up a conversion
-- account for what was converted and nothing else, so the fee has to take
-- itself off the balance.
feePostings ::
  DeclarationSettings ->
  Text ->
  QuantisationFactor ->
  CurrencySymbol ->
  Money.Account ->
  Validation ImportError [Commented () (Posting ())]
feePostings settings transferId qf currency fee =
  if fee == Account.zero
    then pure []
    else do
      offBalance <-
        posting
          transferId
          (declarationSettingAssetsAccountName settings)
          qf
          currency
          (Account.negate fee)
          Nothing
      ontoExpenses <-
        feeExpensePosting settings transferId qf currency fee
      pure (offBalance : ontoExpenses)

-- | The expense side of a fee on its own.
feeExpensePosting ::
  DeclarationSettings ->
  Text ->
  QuantisationFactor ->
  CurrencySymbol ->
  Money.Account ->
  Validation ImportError [Commented () (Posting ())]
feeExpensePosting settings transferId qf currency fee =
  if fee == Account.zero
    then pure []
    else do
      ontoExpenses <-
        posting
          transferId
          (declarationSettingFeesAccountName settings)
          qf
          currency
          fee
          Nothing
      pure [ontoExpenses]

posting ::
  Text ->
  AccountName ->
  QuantisationFactor ->
  CurrencySymbol ->
  Money.Account ->
  Maybe (GenLocated () (PriceAnnotation ())) ->
  Validation ImportError (Commented () (Posting ()))
posting transferId accountName qf currency account price = do
  literal <- literalFromAccount transferId qf account
  pure $
    noCommentsOn
      Posting
        { postingReal = True,
          postingAccountName = noLoc accountName,
          postingAccount = noLoc literal,
          postingCurrencySymbol = noLoc currency,
          postingPrice = price,
          postingRatio = Nothing
        }

-- | What the balance stood at after this row, as an assertion.
--
-- This is the check that makes a wrong import fail rather than sit in the
-- ledger looking plausible, so it is written for every row that has one.
rowAssertion ::
  DeclarationSettings ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Row ->
  Validation ImportError [Commented () (TransactionExtra ())]
rowAssertion settings currencies row = do
  let transferId = rowId row
  qf <- quantisationFactorFor currencies transferId (rowCurrency row)
  balance <- traverse (accountFromLiteral transferId qf) (rowRunningBalance row)
  literal <- traverse (literalFromAccount transferId qf) balance
  pure
    [ noCommentsOn $
        TransactionAssertion $
          noLoc $
            ExtraAssertion $
              noLoc $
                AssertionEquals
                  AssertionScopeReal
                  (noLoc (declarationSettingAssetsAccountName settings))
                  (noLoc l)
                  (noLoc (CommodityExpressionCurrency (noLoc (rowCurrency row))))
    | l <- maybeToList literal
    ]

quantisationFactorFor ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Text ->
  CurrencySymbol ->
  Validation ImportError QuantisationFactor
quantisationFactorFor currencies transferId symbol = case M.lookup symbol currencies of
  Nothing -> validationFailure $ ImportErrorUnknownCurrency transferId symbol
  Just (Located _ qf) -> pure qf

accountFromLiteral ::
  Text ->
  QuantisationFactor ->
  DecimalLiteral ->
  Validation ImportError Money.Account
accountFromLiteral transferId qf dl = case Account.fromDecimalLiteral qf dl of
  Nothing -> validationFailure $ ImportErrorInvalidAccount transferId qf dl
  Just a -> pure a

literalFromAccount ::
  Text ->
  QuantisationFactor ->
  Money.Account ->
  Validation ImportError DecimalLiteral
literalFromAccount transferId qf a = case Account.toDecimalLiteral qf a of
  Nothing -> validationFailure $ ImportErrorInvalidLiteral transferId qf a
  Just dl -> pure dl

addAccounts ::
  Text ->
  Money.Account ->
  Money.Account ->
  Validation ImportError Money.Account
addAccounts transferId a1 a2 = case Account.add a1 a2 of
  Nothing -> validationFailure $ ImportErrorAdd transferId a1 a2
  Just a -> pure a

-- | The rate to write on the posting money left, as the exact ratio of the two
-- amounts.
--
-- Wise states a rate of its own, rounded to a fixed number of decimals.
-- Multiplying by that rate does not give back the amount that arrived, so a
-- transaction written with it would not balance.  The ratio of the two amounts
-- always does, because it is what actually happened.
conversionPrice ::
  -- | The currency money arrived in
  CurrencySymbol ->
  -- | What left, which is negative
  Money.Account ->
  -- | What arrived
  Money.Account ->
  Maybe (PriceAnnotation ())
conversionPrice arrivedCurrency leftAccount arrivedAccount =
  let numerator = abs (Account.toMinimalQuantisations arrivedAccount)
      denominator = abs (Account.toMinimalQuantisations leftAccount)
   in if numerator == 0 || denominator == 0
        then Nothing
        else
          Just $
            PriceAnnotationCost $
              noLoc
                CostExpression
                  { costExpressionConversionRate =
                      noLoc
                        RationalExpression
                          { rationalExpressionNumerator = noLoc (DecimalLiteral.fromInteger numerator),
                            rationalExpressionDenominator = Just (noLoc (DecimalLiteral.fromInteger denominator)),
                            rationalExpressionPercent = False
                          },
                    costExpressionCurrencySymbol = noLoc arrivedCurrency
                  }

-- | What to write on the description line.
eventDescription :: WiseEvent -> Maybe Description
eventDescription event =
  Description.combine $
    map Description $
      case event of
        WiseMovement row -> rowDescriptionParts row
        WiseConversion out back -> case rowDescriptionParts out of
          [] ->
            [ T.unwords
                [ "Convert",
                  currencySymbolText (rowCurrency out),
                  "to",
                  currencySymbolText (rowCurrency back)
                ]
            ]
          parts -> parts

-- | The text a row has to say about itself, in the order a reader wants it.
--
-- A description is made of non-empty lines, so anything that would break that
-- is flattened rather than dropped: a statement cell can hold a newline, and a
-- description that holds one cannot be written to a file and read back.
rowDescriptionParts :: Row -> [Text]
rowDescriptionParts Row {..} =
  filter (not . T.null) $
    nubOrd $
      map oneLine [rowDescription, rowMerchant, rowPaymentReference]

oneLine :: Text -> Text
oneLine = T.strip . T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)
