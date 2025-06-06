{-# LANGUAGE ApplicativeDo #-}
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
  )
where

import Centjes.Compile
import Centjes.Filter (Filter (..))
import Centjes.Ledger
import Centjes.Location
import Centjes.Module as Module
import Centjes.Report.Balance
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
import Error.Diagnose
import Myers.Diff as Diff
import Path
import Path.IO

type CheckerT ann a = ValidationT (CheckError ann) (LoggingT IO) a

type Checker ann a = Validation (CheckError ann) a

doCompleteCheck ::
  [LDeclaration] ->
  CheckerT
    SourceSpan
    ( Ledger SourceSpan,
      BalanceReport SourceSpan,
      Register SourceSpan
    )
doCompleteCheck declarations = do
  () <- withLoggedDuration "Check declarations" $ checkLDeclarations declarations
  ledger <- withLoggedDuration "Compile" $ liftValidation $ mapValidationFailure CheckErrorCompileError $ compileDeclarations declarations
  (balanceReport, register) <- withLoggedDuration "Check ledger" $ liftValidation $ checkLedger ledger
  pure (ledger, balanceReport, register)

data CheckError ann
  = CheckErrorDeclarationOutOfOrder !(GenLocated ann Timestamp) !(GenLocated ann Timestamp)
  | CheckErrorMissingAttachment !ann !(Attachment ann) ![Path Rel File]
  | CheckErrorUnusedCurrency !(GenLocated ann (CurrencyDeclaration ann))
  | CheckErrorUnusedAccount !(GenLocated ann (AccountDeclaration ann))
  | CheckErrorUnusedTag !(GenLocated ann (TagDeclaration ann))
  | CheckErrorCompileError !(CompileError ann)
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
        [ Hint $ intercalate "\n" $ "Perhaps it was a typo and you meant one of these files in the same directory:" : map fromRelFile fs
          | not (null fs)
        ]
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
    CheckErrorCompileError ce -> toReport ce
    CheckErrorBalanceError be -> toReport be
    CheckErrorRegisterError re -> toReport re

checkLDeclarations :: [LDeclaration] -> CheckerT SourceSpan ()
checkLDeclarations = checkDeclarations . map locatedValue

checkDeclarations :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDeclarations declarations = do
  withLoggedDuration "Check declaration ordering" $ checkDeclarationOrdering declarations
  withLoggedDuration "Check currency usage" $ checkCurrencyUsage declarations
  withLoggedDuration "Check account usage" $ checkAccountUsage declarations
  withLoggedDuration "Check tag usage" $ checkTagUsage declarations
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
                    ( \(Located _ Module.Posting {..}) ->
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
                        Located _ (AccountExtraAttachment _) ->
                          S.empty
                        Located _ (AccountExtraAssertion _) ->
                          S.empty
                        Located _ (AccountExtraTag (Located _ (ExtraTag (Located _ tag)))) ->
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
                        Located _ (TransactionAttachment _) ->
                          S.empty
                        Located _ (TransactionAssertion _) ->
                          S.empty
                        Located _ (TransactionTag (Located _ (ExtraTag (Located _ tag)))) ->
                          S.singleton tag
                    )
                    transactionExtras
           in (ds, S.union tags us)

      (declared, used) = foldl' go (M.empty, S.empty) declarations
      unuseds = M.difference declared $ M.fromSet (const ()) used
   in for_ unuseds $ \unused ->
        validationTFailure $ CheckErrorUnusedTag unused

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
  traverse_ (checkAccountExtra al . locatedValue) accountDeclarationExtras

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
  traverse_ (checkTransactionExtra tl . locatedValue) transactionExtras

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
    fs <- take 10 . map fst . sortOn snd . mapMaybe similarity . snd <$> listDirRel (parent af)
    validationTFailure $ CheckErrorMissingAttachment tl a fs

checkLedger ::
  (Ord ann) =>
  Ledger ann ->
  Checker ann (BalanceReport ann, Register ann)
checkLedger l = do
  balanceReport <-
    mapValidationFailure CheckErrorBalanceError $
      produceBalanceReport
        FilterAny
        Nothing
        Nothing
        False
        l
  register <-
    mapValidationFailure CheckErrorRegisterError $
      produceRegister
        FilterAny
        Nothing
        False
        Nothing
        Nothing
        l
  pure (balanceReport, register)
