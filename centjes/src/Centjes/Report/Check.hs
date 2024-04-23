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
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import Error.Diagnose
import Path
import Path.IO

type CheckerT ann a = ValidationT (CheckError ann) IO a

type Checker ann a = Validation (CheckError ann) a

doCompleteCheck ::
  [Declaration SourceSpan] ->
  CheckerT
    SourceSpan
    ( Ledger SourceSpan,
      BalanceReport SourceSpan,
      Register SourceSpan
    )
doCompleteCheck declarations = do
  () <- checkDeclarations declarations
  ledger <- liftValidation $ mapValidationFailure CheckErrorCompileError $ compileDeclarations declarations
  () <- checkDeclarations declarations
  (balanceReport, register) <- liftValidation $ checkLedger ledger
  pure (ledger, balanceReport, register)

data CheckError ann
  = CheckErrorDeclarationOutOfOrder !(GenLocated ann Timestamp) !(GenLocated ann Timestamp)
  | CheckErrorMissingAttachment !ann !(Attachment ann)
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
    CheckErrorMissingAttachment tl (Attachment (Located fl fp)) ->
      Err
        (Just "CE_MISSING_ATTACHMENT")
        (unwords ["Attachment does not exist:", show fp])
        [ (toDiagnosePosition tl, Where "While trying to check this transaction"),
          (toDiagnosePosition fl, This "This file is missing")
        ]
        []
    CheckErrorCompileError ce -> toReport ce
    CheckErrorBalanceError be -> toReport be
    CheckErrorRegisterError re -> toReport re

checkDeclarations :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDeclarations declarations = do
  checkDeclarationOrdering declarations
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

checkDeclaration :: Declaration SourceSpan -> CheckerT SourceSpan ()
checkDeclaration = \case
  DeclarationComment _ -> pure ()
  DeclarationCurrency _ -> pure ()
  DeclarationAccount _ -> pure ()
  DeclarationTag _ -> pure ()
  DeclarationPrice _ -> pure ()
  DeclarationTransaction t -> checkTransaction t

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

checkAttachment :: SourceSpan -> LAttachment -> CheckerT SourceSpan ()
checkAttachment tl (Located _ a@(Attachment (Located l fp))) = do
  let base = sourceSpanBase l
  let af = base </> fp
  exists <- liftIO $ doesFileExist af
  -- TODO error when attachment exists but is not readable.
  when (not exists) $ validationTFailure $ CheckErrorMissingAttachment tl a

checkLedger ::
  (Ord ann) =>
  Ledger ann ->
  Checker ann (BalanceReport ann, Register ann)
checkLedger l = do
  balanceReport <- mapValidationFailure CheckErrorBalanceError $ produceBalanceReport FilterAny Nothing l
  register <- mapValidationFailure CheckErrorRegisterError $ produceRegister FilterAny Nothing l
  pure (balanceReport, register)
