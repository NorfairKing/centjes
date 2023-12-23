{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Check
  ( runCentjesCheck,
    doCompleteCheck,
    CheckError (..),
    checkDeclarations,
  )
where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Module as Module
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Report.Register
import Centjes.Validation
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import Data.Validity (Validity)
import Error.Diagnose
import GHC.Generics (Generic)
import Path
import Path.IO

runCentjesCheck :: Settings -> CheckSettings -> IO ()
runCentjesCheck Settings {..} CheckSettings = runStderrLoggingT $ do
  (declarations, diag) <- loadModules settingLedgerFile
  val <- liftIO $ runValidationT $ doCompleteCheck declarations
  void $ liftIO $ checkValidation diag val

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
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (CheckError ann)

instance NFData ann => NFData (CheckError ann)

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
  DeclarationPrice _ -> pure ()
  DeclarationTransaction t -> checkTransaction t

checkTransaction :: Located (Module.Transaction SourceSpan) -> CheckerT SourceSpan ()
checkTransaction (Located tl Module.Transaction {..}) = do
  traverse_ (checkTransactionExtra tl . locatedValue) transactionExtras

checkTransactionExtra :: SourceSpan -> TransactionExtra SourceSpan -> CheckerT SourceSpan ()
checkTransactionExtra tl = \case
  TransactionAttachment a -> checkAttachment tl a
  TransactionAssertion _ -> pure ()

checkAttachment :: SourceSpan -> LAttachment -> CheckerT SourceSpan ()
checkAttachment tl (Located _ a@(Attachment (Located l fp))) = do
  let base = sourceSpanBase l
  let curFile = sourceSpanFile l
  let af = base </> parent curFile </> fp
  exists <- liftIO $ doesFileExist af
  -- TODO error when attachment exists but is not readable.
  when (not exists) $ validationTFailure $ CheckErrorMissingAttachment tl a

checkLedger :: Ord ann => Ledger ann -> Checker ann (BalanceReport ann, Register ann)
checkLedger l = do
  balanceReport <- mapValidationFailure CheckErrorBalanceError $ produceBalanceReport Nothing l
  register <- mapValidationFailure CheckErrorRegisterError $ produceRegister Nothing l
  pure (balanceReport, register)
