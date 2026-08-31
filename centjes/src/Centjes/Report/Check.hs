{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Check
  ( doCompleteCheck,
    CheckError (..),
    checkDeclarations,
    duplicateAttachmentTag,
  )
where

import Centjes.Compile
import Centjes.Format (formatTransactionExtra)
import Centjes.Ledger
import Centjes.Location
import Centjes.Module as Module
import Centjes.Report.Balance
import Centjes.Report.EvaluatedLedger
import Centjes.Report.Register
import qualified Centjes.Timestamp as Timestamp
import Centjes.Timing
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Error.Diagnose
import Myers.Diff as Diff
import Path
import Path.IO
import Text.Colour

{-# ANN module ("DisableMutations" :: String) #-}

type CheckerT ann a = ValidationT (CheckError ann) (LoggingT IO) a

type Checker ann a = Validation (CheckError ann) a

doCompleteCheck ::
  [LDeclaration] ->
  CheckerT
    SourceSpan
    ( Ledger SourceSpan,
      BalanceReport SourceSpan,
      Register 'MultiCurrency SourceSpan
    )
doCompleteCheck declarations = do
  () <- withLoggedDuration "Check declarations" $ checkLDeclarations declarations
  ledger <- withLoggedDuration "Compile" $ liftValidation $ mapValidationFailure CheckErrorCompileError $ compileDeclarations declarations
  (balanceReport, register) <- withLoggedDuration "Check ledger" $ liftValidation $ checkLedger ledger
  pure (ledger, balanceReport, register)

data CheckError ann
  = CheckErrorDeclarationOutOfOrder !(GenLocated ann Timestamp) !(GenLocated ann Timestamp)
  | CheckErrorMissingAttachment !ann !(Attachment ann) ![Path Rel File]
  | CheckErrorDuplicateAttachment !ann !ann !(Path Rel File)
  | CheckErrorUnnecessaryDuplicateAttachmentTag !ann !ann
  | CheckErrorUnusedCurrency !(GenLocated ann (CurrencyDeclaration ann))
  | CheckErrorUnusedAccount !(GenLocated ann (AccountDeclaration ann))
  | CheckErrorUnusedTag !(GenLocated ann (TagDeclaration ann))
  | CheckErrorCompileError !(CompileError ann)
  | CheckErrorEvaluatedLedgerError !(EvaluatedLedgerError ann)
  | CheckErrorBalanceError !(BalanceError ann)
  | CheckErrorRegisterError !(RegisterError ann)

instance ToReport (CheckError SourceSpan) where
  toReport = \case
    CheckErrorDeclarationOutOfOrder (Located ts1l _) (Located ts2l _) ->
      Err
        (Just "CE_DECLARATION_OUT_OF_ORDER")
        "Declarations out of orderI"
        [ (toDiagnosePosition ts1l, This "This is declared before ..."),
          (toDiagnosePosition ts2l, Where "... this, but its timestamp indicates it needs to be declared after.")
        ]
        []
    CheckErrorMissingAttachment tl (Attachment (Located fl fp)) fs ->
      Err
        (Just "CE_MISSING_ATTACHMENT")
        (unwords ["Attachment does not exist:", show fp])
        [ (toDiagnosePosition tl, Where "While trying to check this transaction"),
          (toDiagnosePosition fl, This "This file is missing")
        ]
        [ Hint $
            intercalate "\n" $
              "Perhaps it was a typo and you meant one of these files in the same directory:"
                : map (renderAttachmentSuggestion (fromRelFile (filename fp))) fs
        | not (null fs)
        ]
    CheckErrorDuplicateAttachment l1 l2 rf ->
      Err
        (Just "CE_DUPLICATE_ATTACHMENT")
        (unwords ["Duplicate attachment:", fromRelFile rf])
        [ (toDiagnosePosition l1, Where "This attachment is the same as ..."),
          (toDiagnosePosition l2, This "... this attachment")
        ]
        [Hint $ "If this is intentional, add '" <> T.unpack (formatTransactionExtra duplicateAttachmentExtra) <> "' to both."]
    CheckErrorUnnecessaryDuplicateAttachmentTag dl tl ->
      Err
        (Just "CE_UNNECESSARY_DUPLICATE_ATTACHMENT_TAG")
        (unwords ["Unnecessary tag:", T.unpack (tagText duplicateAttachmentTag)])
        [ (toDiagnosePosition dl, Where "While trying to check this declaration"),
          (toDiagnosePosition tl, This "This tag claims that an attachment is attached more than once, but none of the attachments here are.")
        ]
        [Hint "Either remove this tag or attach an attachment that is attached elsewhere too."]
    CheckErrorUnusedCurrency (Located dl _) ->
      Err
        (Just "CE_UNUSED_CURRENCY")
        "This currency has been declared but is never used."
        [(toDiagnosePosition dl, This "This currency is declared here but is never used.")]
        [Hint "Either use it or delete this declaration."]
    CheckErrorUnusedAccount (Located dl _) ->
      Err
        (Just "CE_UNUSED_ACCOUNT")
        "This account has been declared but is never used."
        [(toDiagnosePosition dl, This "This account is declared here but is never used.")]
        [Hint "Either use it or delete this declaration."]
    CheckErrorUnusedTag (Located dl _) ->
      Err
        (Just "CE_UNUSED_TAG")
        "This tag has been declared but is never used."
        [(toDiagnosePosition dl, This "This tag is declared here but is never used.")]
        [Hint "Either use it or delete this declaration."]
    CheckErrorCompileError compileError -> toReport compileError
    CheckErrorEvaluatedLedgerError evaluatedLedgerError -> toReport evaluatedLedgerError
    CheckErrorBalanceError balanceError -> toReport balanceError
    CheckErrorRegisterError registerError -> toReport registerError

-- | Render a candidate file name as a character-level diff against the
-- attachment file name that was typed.
--
-- Only characters that are actually part of the candidate file name are shown,
-- so that the rendered text is always a real path.
-- Characters that the candidate has in addition to the typed name are shown in
-- red, and unchanged characters are shown in the same colour as the rest of the
-- hint.
-- We colour the unchanged characters explicitly rather than leaving them
-- uncoloured, because otherwise a coloured difference would reset the colour
-- back to the terminal default for the remaining unchanged characters.
-- This makes it easy to spot which path is the most likely correct one.
renderAttachmentSuggestion :: String -> Path Rel File -> String
renderAttachmentSuggestion typed candidate =
  T.unpack $
    renderChunksText With8Colours $
      mapMaybe characterChunk $
        Diff.getStringDiff typed (fromRelFile (filename candidate))
  where
    characterChunk :: Diff Char -> Maybe Chunk
    characterChunk = \case
      Both character _ -> Just $ fore brightCyan $ chunk (T.singleton character)
      -- Present in the typed name but not in the candidate, so don't show it.
      First _ -> Nothing
      Second character -> Just $ fore red $ chunk (T.singleton character)

checkLDeclarations :: [LDeclaration] -> CheckerT SourceSpan ()
checkLDeclarations = checkDeclarations . map locatedValue

checkDeclarations :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDeclarations declarations = do
  withLoggedDuration "Check declaration ordering" $ checkDeclarationOrdering declarations
  withLoggedDuration "Check currency usage" $ checkCurrencyUsage declarations
  withLoggedDuration "Check account usage" $ checkAccountUsage declarations
  withLoggedDuration "Check tag usage" $ checkTagUsage declarations
  withLoggedDuration "Check duplicate attachments" $ checkDuplicateAttachments declarations
  -- Check declarations individually
  traverse_ checkDeclaration declarations

checkDeclarationOrdering :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDeclarationOrdering = go . mapMaybe timestampAndSource
  where
    timestampAndSource :: Declaration SourceSpan -> Maybe (Located Timestamp)
    timestampAndSource = \case
      DeclarationComment _ -> Nothing
      DeclarationCurrency _ -> Nothing
      DeclarationAccount _ -> Nothing
      DeclarationTag _ -> Nothing
      DeclarationPrice (Located _ Module.PriceDeclaration {..}) -> Just priceDeclarationTimestamp
      DeclarationTransaction (Located _ Module.Transaction {..}) -> Just transactionTimestamp
    go :: [Located Timestamp] -> CheckerT SourceSpan ()
    go = \case
      [] -> pure ()
      [_] -> pure ()
      (lt1@(Located s1 ts1) : lt2@(Located s2 ts2) : rest) -> do
        -- If declarations are in the same file but out of order, error
        let checkFirstTup = when (sourceSpanFile s1 == sourceSpanFile s2) $
              case Timestamp.comparePartially ts1 ts2 of
                Just GT -> validationTFailure $ CheckErrorDeclarationOutOfOrder lt1 lt2
                _ -> pure ()
        let checkRest = go (lt2 : rest)
        checkFirstTup <* checkRest

checkCurrencyUsage :: forall ann. [Declaration ann] -> CheckerT ann ()
checkCurrencyUsage declarations =
  let go ::
        (Map CurrencySymbol (GenLocated ann (CurrencyDeclaration ann)), Set CurrencySymbol) ->
        Declaration ann ->
        (Map CurrencySymbol (GenLocated ann (CurrencyDeclaration ann)), Set CurrencySymbol)
      go t@(ds, us) = \case
        DeclarationComment _ -> t
        DeclarationCurrency lcd@(Located _ cd) ->
          let Located _ cs = currencyDeclarationSymbol cd
           in (M.insert cs lcd ds, us)
        DeclarationAccount _ -> t
        DeclarationTag _ -> t
        DeclarationPrice (Located _ pdl) ->
          let currencys = priceDeclarationCurrencySymbols pdl
           in (ds, S.union currencys us)
        DeclarationTransaction (Located _ transaction) ->
          let currencys = transactionCurrencySymbols transaction
           in (ds, S.union currencys us)

      (declared, used) = foldl' go (M.empty, S.empty) declarations
      unuseds = M.difference declared $ M.fromSet (const ()) used
   in for_ unuseds $ \unused ->
        validationTFailure $ CheckErrorUnusedCurrency unused

checkAccountUsage :: forall ann. [Declaration ann] -> CheckerT ann ()
checkAccountUsage declarations =
  let go ::
        (Map AccountName (GenLocated ann (AccountDeclaration ann)), Set AccountName) ->
        Declaration ann ->
        (Map AccountName (GenLocated ann (AccountDeclaration ann)), Set AccountName)
      go t@(ds, us) = \case
        DeclarationComment _ -> t
        DeclarationCurrency _ -> t
        DeclarationAccount lad@(Located _ ad) ->
          let Located _ an = accountDeclarationName ad
           in (M.insert an lad ds, us)
        DeclarationTag _ -> t
        DeclarationPrice _ -> t
        DeclarationTransaction (Located _ Module.Transaction {..}) ->
          let accounts =
                S.fromList $
                  map
                    ( \(Commented (Located _ Module.Posting {..}) _) ->
                        locatedValue postingAccountName
                    )
                    transactionPostings
           in (ds, S.union accounts us)

      (declared, used) = foldl' go (M.empty, S.empty) declarations
      unuseds = M.difference declared $ M.fromSet (const ()) used
   in for_ unuseds $ \unused ->
        validationTFailure $ CheckErrorUnusedAccount unused

checkTagUsage :: forall ann. [Declaration ann] -> CheckerT ann ()
checkTagUsage declarations =
  let go ::
        (Map Tag (GenLocated ann (TagDeclaration ann)), Set Tag) ->
        Declaration ann ->
        (Map Tag (GenLocated ann (TagDeclaration ann)), Set Tag)
      go t@(ds, us) = \case
        DeclarationComment _ -> t
        DeclarationCurrency _ -> t
        DeclarationAccount (Located _ Module.AccountDeclaration {..}) ->
          let tags =
                S.unions $
                  map
                    ( \case
                        Commented (Located _ (AccountExtraAttachment _)) _ ->
                          S.empty
                        Commented (Located _ (AccountExtraAssertion _)) _ ->
                          S.empty
                        Commented (Located _ (AccountExtraTag (Located _ (ExtraTag (Located _ tag))))) _ ->
                          S.singleton tag
                    )
                    accountDeclarationExtras
           in (ds, S.union tags us)
        DeclarationTag ltd@(Located _ td) ->
          let Located _ tn = tagDeclarationTag td
           in (M.insert tn ltd ds, us)
        DeclarationPrice _ -> t
        DeclarationTransaction (Located _ Module.Transaction {..}) ->
          let tags =
                S.unions $
                  map
                    ( \case
                        Commented (Located _ (TransactionAttachment _)) _ ->
                          S.empty
                        Commented (Located _ (TransactionAssertion _)) _ ->
                          S.empty
                        Commented (Located _ (TransactionTag (Located _ (ExtraTag (Located _ tag))))) _ ->
                          S.singleton tag
                    )
                    transactionExtras
           in (ds, S.union tags us)

      (declared, used) = foldl' go (M.empty, S.empty) declarations
      unuseds = M.difference declared $ M.fromSet (const ()) used
   in for_ unuseds $ \unused ->
        validationTFailure $ CheckErrorUnusedTag unused

duplicateAttachmentTag :: Tag
duplicateAttachmentTag = "duplicate-attachment"

duplicateAttachmentExtra :: TransactionExtra ()
duplicateAttachmentExtra = TransactionTag (Located () (ExtraTag (Located () duplicateAttachmentTag)))

-- | Where one attachment is attached, resolved so that two attachments of the
-- same file from different directories compare equal.
data AttachmentOccurrence ann = AttachmentOccurrence
  { attachmentOccurrenceAbsolutePath :: !(Path Abs File),
    attachmentOccurrenceRelativePath :: !(Path Rel File),
    attachmentOccurrenceLocation :: !ann
  }

-- | The attachments of a single declaration, together with the location of its
-- 'duplicateAttachmentTag' if it has one.
data DeclarationAttachments ann = DeclarationAttachments
  { declarationAttachmentsDeclarationLocation :: !ann,
    declarationAttachmentsDuplicateTagLocation :: !(Maybe ann),
    declarationAttachmentsOccurrences :: ![AttachmentOccurrence ann]
  }

checkDuplicateAttachments :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDuplicateAttachments declarations =
  let perDeclaration :: [DeclarationAttachments SourceSpan]
      perDeclaration = mapMaybe declarationAttachments declarations
      -- Counted over every declaration, tagged ones included, so that a tag is
      -- justified by the attachment it excuses rather than by itself.
      occurrenceCounts :: Map (Path Abs File) Int
      occurrenceCounts =
        M.fromListWith (+) $
          map (\o -> (attachmentOccurrenceAbsolutePath o, 1)) $
            concatMap declarationAttachmentsOccurrences perDeclaration
      isDuplicated :: AttachmentOccurrence SourceSpan -> Bool
      isDuplicated o = M.findWithDefault 0 (attachmentOccurrenceAbsolutePath o) occurrenceCounts > 1
      checkTag :: DeclarationAttachments SourceSpan -> CheckerT SourceSpan ()
      checkTag da =
        for_ (declarationAttachmentsDuplicateTagLocation da) $ \tl ->
          unless (any isDuplicated (declarationAttachmentsOccurrences da)) $
            validationTFailure $
              CheckErrorUnnecessaryDuplicateAttachmentTag
                (declarationAttachmentsDeclarationLocation da)
                tl
      untagged :: [AttachmentOccurrence SourceSpan]
      untagged =
        concatMap
          ( \da -> case declarationAttachmentsDuplicateTagLocation da of
              Just _ -> []
              Nothing -> declarationAttachmentsOccurrences da
          )
          perDeclaration
      go ::
        Map (Path Abs File) (AttachmentOccurrence SourceSpan) ->
        AttachmentOccurrence SourceSpan ->
        CheckerT SourceSpan (Map (Path Abs File) (AttachmentOccurrence SourceSpan))
      go seen o =
        case M.lookup (attachmentOccurrenceAbsolutePath o) seen of
          Nothing -> pure $ M.insert (attachmentOccurrenceAbsolutePath o) o seen
          Just previous -> do
            validationTFailure $
              CheckErrorDuplicateAttachment
                (attachmentOccurrenceLocation previous)
                (attachmentOccurrenceLocation o)
                (attachmentOccurrenceRelativePath o)
            pure seen
   in traverse_ checkTag perDeclaration *> foldM_ go M.empty untagged

declarationAttachments :: Declaration SourceSpan -> Maybe (DeclarationAttachments SourceSpan)
declarationAttachments = \case
  DeclarationComment _ -> Nothing
  DeclarationCurrency _ -> Nothing
  DeclarationTag _ -> Nothing
  DeclarationPrice _ -> Nothing
  DeclarationAccount (Located dl Module.AccountDeclaration {..}) ->
    Just
      DeclarationAttachments
        { declarationAttachmentsDeclarationLocation = dl,
          declarationAttachmentsDuplicateTagLocation =
            listToMaybe $ mapMaybe (accountExtraDuplicateTag . commentedValue) accountDeclarationExtras,
          declarationAttachmentsOccurrences =
            mapMaybe (accountExtraAttachment . commentedValue) accountDeclarationExtras
        }
  DeclarationTransaction (Located dl Module.Transaction {..}) ->
    Just
      DeclarationAttachments
        { declarationAttachmentsDeclarationLocation = dl,
          declarationAttachmentsDuplicateTagLocation =
            listToMaybe $ mapMaybe (transactionExtraDuplicateTag . commentedValue) transactionExtras,
          declarationAttachmentsOccurrences =
            mapMaybe (transactionExtraAttachment . commentedValue) transactionExtras
        }
  where
    accountExtraDuplicateTag :: Located (AccountExtra SourceSpan) -> Maybe SourceSpan
    accountExtraDuplicateTag (Located el ae) = case ae of
      AccountExtraAttachment _ -> Nothing
      AccountExtraAssertion _ -> Nothing
      AccountExtraTag (Located _ (ExtraTag (Located _ tag))) ->
        if tag == duplicateAttachmentTag then Just el else Nothing
    transactionExtraDuplicateTag :: Located (TransactionExtra SourceSpan) -> Maybe SourceSpan
    transactionExtraDuplicateTag (Located el te) = case te of
      TransactionAttachment _ -> Nothing
      TransactionAssertion _ -> Nothing
      TransactionTag (Located _ (ExtraTag (Located _ tag))) ->
        if tag == duplicateAttachmentTag then Just el else Nothing
    accountExtraAttachment :: Located (AccountExtra SourceSpan) -> Maybe (AttachmentOccurrence SourceSpan)
    accountExtraAttachment (Located _ ae) = case ae of
      AccountExtraAttachment (Located _ (ExtraAttachment (Located _ (Attachment (Located l fp))))) ->
        Just (attachmentOccurrence l fp)
      AccountExtraAssertion _ -> Nothing
      AccountExtraTag _ -> Nothing
    transactionExtraAttachment :: Located (TransactionExtra SourceSpan) -> Maybe (AttachmentOccurrence SourceSpan)
    transactionExtraAttachment (Located _ te) = case te of
      TransactionAttachment (Located _ (ExtraAttachment (Located _ (Attachment (Located l fp))))) ->
        Just (attachmentOccurrence l fp)
      TransactionAssertion _ -> Nothing
      TransactionTag _ -> Nothing
    attachmentOccurrence :: SourceSpan -> Path Rel File -> AttachmentOccurrence SourceSpan
    attachmentOccurrence l fp =
      AttachmentOccurrence
        { attachmentOccurrenceAbsolutePath = sourceSpanBase l </> fp,
          attachmentOccurrenceRelativePath = fp,
          attachmentOccurrenceLocation = l
        }

checkDeclaration :: Declaration SourceSpan -> CheckerT SourceSpan ()
checkDeclaration = \case
  DeclarationComment _ -> pure ()
  DeclarationCurrency _ -> pure ()
  DeclarationAccount a -> checkAccount a
  DeclarationTag _ -> pure ()
  DeclarationPrice _ -> pure ()
  DeclarationTransaction t -> checkTransaction t

checkAccount :: Located (Module.AccountDeclaration SourceSpan) -> CheckerT SourceSpan ()
checkAccount (Located al Module.AccountDeclaration {..}) = do
  traverse_ (checkAccountExtra al . locatedValue . commentedValue) accountDeclarationExtras

checkAccountExtra ::
  SourceSpan ->
  AccountExtra SourceSpan ->
  CheckerT SourceSpan ()
checkAccountExtra tl = \case
  AccountExtraAttachment a -> checkAttachment tl a
  AccountExtraAssertion _ -> pure ()
  AccountExtraTag _ -> pure ()

checkTransaction :: Located (Module.Transaction SourceSpan) -> CheckerT SourceSpan ()
checkTransaction (Located tl Module.Transaction {..}) = do
  traverse_ (checkTransactionExtra tl . locatedValue . commentedValue) transactionExtras

checkTransactionExtra ::
  SourceSpan ->
  TransactionExtra SourceSpan ->
  CheckerT SourceSpan ()
checkTransactionExtra tl = \case
  TransactionAttachment a -> checkAttachment tl a
  TransactionAssertion _ -> pure ()
  TransactionTag _ -> pure ()

checkAttachment :: SourceSpan -> LExtraAttachment -> CheckerT SourceSpan ()
checkAttachment tl (Located _ (ExtraAttachment (Located _ a@(Attachment (Located l fp))))) = do
  let base = sourceSpanBase l
  let af = base </> fp
  exists <- liftIO $ doesFileExist af

  -- TODO also error when attachment exists but is not readable.
  when (not exists) $ do
    -- Show the (up to 10) most similar looking files in the same directory
    let patt = fromRelFile (filename af)
    let similarity p = do
          let diff = Diff.getStringDiff patt (fromRelFile (filename p))
          let penalty =
                length $
                  filter
                    ( \case
                        First _ -> True
                        Second _ -> True
                        Both {} -> False
                    )
                    diff
          guard $ penalty < length patt
          pure (p, penalty)
    -- The parent directory may not exist either (for example because of a typo
    -- in the path), in which case there are simply no similar files to suggest.
    mFiles <- liftIO $ forgivingAbsence $ snd <$> listDirRel (parent af)
    -- Sort by name as well as penalty, because the order in which the files
    -- are listed is filesystem-dependent.
    let fs = take 10 . map fst . sortOn (\(p, penalty) -> (penalty, p)) . mapMaybe similarity $ fromMaybe [] mFiles
    validationTFailure $ CheckErrorMissingAttachment tl a fs

checkLedger ::
  (Ord ann) =>
  Ledger ann ->
  Checker ann (BalanceReport ann, Register 'MultiCurrency ann)
checkLedger ledger = do
  -- Produce the evaluated ledger once, shared by both reports
  evaluatedLedger <-
    mapValidationFailure CheckErrorEvaluatedLedgerError $
      produceEvaluatedLedger ledger
  -- Run assertion checks (account type assertions and explicit assertions)
  mapValidationFailure CheckErrorEvaluatedLedgerError $
    checkEvaluatedLedgerAssertions evaluatedLedger
  balanceReport <-
    mapValidationFailure CheckErrorBalanceError $
      produceBalanceReportFromEvaluatedLedger
        FilterAny
        Nothing
        Nothing
        False
        evaluatedLedger
  register <-
    mapValidationFailure CheckErrorRegisterError $
      produceMultiCurrencyRegister
        FilterAny
        BlockSizeIndividual
        False
        Nothing
        Nothing
        evaluatedLedger
  pure (balanceReport, register)
