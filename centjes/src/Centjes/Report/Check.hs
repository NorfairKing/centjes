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
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
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
  (balanceReport, register) <- liftValidation $ checkLedger ledger
  pure (ledger, balanceReport, register)

data CheckError ann
  = CheckErrorMissingAttachment !ann !(Attachment ann)
  | CheckErrorCompileError !(CompileError ann)
  | CheckErrorBalanceError !(BalanceError ann)
  | CheckErrorRegisterError !(RegisterError ann)

instance ToReport (CheckError SourceSpan) where
  toReport = \case
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
checkDeclarations = traverse_ checkDeclaration

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
