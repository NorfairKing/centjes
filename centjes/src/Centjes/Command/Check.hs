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
    checkAccounts,
    checkAccountsUnique,
    checkAccountsDeclared,
  )
where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Module as Module
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Validation
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Validity (Validity)
import Error.Diagnose
import GHC.Generics (Generic)
import Path
import Path.IO

runCentjesCheck :: Settings -> CheckSettings -> IO ()
runCentjesCheck Settings {..} CheckSettings = runStderrLoggingT $ do
  (declarations, diag) <- loadModules settingLedgerFile
  val <- liftIO $ runValidationT $ doCompleteCheck declarations
  liftIO $ checkValidation diag val

type CheckerT ann a = ValidationT (CheckError ann) IO a

type Checker ann a = Validation (CheckError ann) a

doCompleteCheck :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
doCompleteCheck declarations = do
  () <- checkDeclarations declarations
  ledger <- liftValidation $ mapValidationFailure CheckErrorCompileError $ compileDeclarations declarations
  liftValidation $ checkLedger ledger

data CheckError ann
  = CheckErrorAccountDeclaredTwice !ann !ann !AccountName
  | CheckErrorUndeclaredAccount !ann !ann !(GenLocated ann AccountName)
  | CheckErrorMissingAttachment !ann !(GenLocated ann Attachment)
  | CheckErrorCompileError !(CompileError ann)
  | CheckErrorBalanceError !(BalanceError ann)
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (CheckError ann)

instance NFData ann => NFData (CheckError ann)

instance ToReport (CheckError SourceSpan) where
  toReport = \case
    CheckErrorAccountDeclaredTwice adl1 adl2 an ->
      Err
        (Just "CE_DUPLICATE_ACCOUNT")
        ( unwords
            [ "Account has been declared twice:",
              show (accountNameText an)
            ]
        )
        [ (toDiagnosePosition adl1, Where "This account has been declared here first"),
          (toDiagnosePosition adl2, This "This account has been declared twice")
        ]
        []
    CheckErrorUndeclaredAccount tl pl (Located al an) ->
      Err
        (Just "CE_UNDECLARED_ACCOUNT")
        ( unwords
            [ "Account has not been declared:",
              show (accountNameText an)
            ]
        )
        [ (toDiagnosePosition tl, Where "While trying to check this transaction"),
          (toDiagnosePosition al, This "This account has not been declared"),
          (toDiagnosePosition pl, Where "While trying to check this posting")
        ]
        []
    CheckErrorMissingAttachment tl (Located al (Attachment fp)) ->
      Err
        (Just "CE_MISSING_ATTACHMENT")
        (unwords ["Attachment does not exist:", show fp])
        [ (toDiagnosePosition tl, Where "While trying to check this transaction"),
          (toDiagnosePosition al, This "This attachment is missing")
        ]
        []
    CheckErrorCompileError ce -> toReport ce
    CheckErrorBalanceError be -> toReport be

checkDeclarations :: [Declaration SourceSpan] -> CheckerT SourceSpan ()
checkDeclarations ds = do
  liftValidation $ checkAccounts ds
  traverse_ checkDeclaration ds

checkAccounts :: [Declaration ann] -> Checker ann ()
checkAccounts ds = do
  as <- checkAccountsUnique ds
  checkAccountsDeclared as ds

checkAccountsUnique :: [Declaration ann] -> Validation (CheckError ann) (Map AccountName ann)
checkAccountsUnique ds =
  validateUniquesMap CheckErrorAccountDeclaredTwice $
    mapMaybe
      ( \case
          DeclarationAccount (Located l AccountDeclaration {..}) -> Just (locatedValue accountDeclarationName, l)
          _ -> Nothing
      )
      ds

checkAccountsDeclared :: Map AccountName ann -> [Declaration ann] -> Checker ann ()
checkAccountsDeclared as = traverse_ $ \case
  DeclarationComment _ -> pure ()
  DeclarationCurrency _ -> pure ()
  DeclarationAccount _ -> pure ()
  DeclarationTransaction (Located tl t) -> for_ (Module.transactionPostings t) $ \(Located pl p) ->
    let Located _ an = Module.postingAccountName p
     in case M.lookup an as of
          Just _ -> pure ()
          Nothing -> validationFailure $ CheckErrorUndeclaredAccount tl pl (Module.postingAccountName p)

validateUniquesMap ::
  forall a l e.
  Ord a =>
  (l -> l -> a -> e) ->
  [(a, l)] ->
  Validation e (Map a l)
validateUniquesMap errFunc = foldM go M.empty
  where
    go :: Map a l -> (a, l) -> Validation e (Map a l)
    go m (a, l) = case M.lookup a m of
      Nothing -> pure $ M.insert a l m
      Just l' -> validationFailure $ errFunc l' l a

checkDeclaration :: Declaration SourceSpan -> CheckerT SourceSpan ()
checkDeclaration = \case
  DeclarationComment _ -> pure ()
  DeclarationCurrency _ -> pure ()
  DeclarationAccount _ -> pure ()
  DeclarationTransaction t -> checkTransaction t

checkTransaction :: Located (Module.Transaction SourceSpan) -> CheckerT SourceSpan ()
checkTransaction (Located tl Module.Transaction {..}) = do
  traverse_ (checkAttachment tl) transactionAttachments

checkAttachment :: SourceSpan -> Located Attachment -> CheckerT SourceSpan ()
checkAttachment tl a@(Located l (Attachment fp)) = do
  let base = sourceSpanBase l
  let curFile = sourceSpanFile l
  let af = base </> parent curFile </> fp
  exists <- liftIO $ doesFileExist af
  -- TODO error when attachment exists but is not readable.
  when (not exists) $ validationTFailure $ CheckErrorMissingAttachment tl a

checkLedger :: Ord ann => Ledger ann -> Checker ann ()
checkLedger l = do
  _ <- mapValidationFailure CheckErrorBalanceError $ produceBalanceReport l
  pure ()
