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

runCentjesCheck :: Settings -> CheckSettings -> IO ()
runCentjesCheck Settings {..} CheckSettings = runStderrLoggingT $ do
  (declarations, diag) <- loadModules settingLedgerFile
  liftIO $ checkValidation diag $ doCompleteCheck declarations

doCompleteCheck :: Ord ann => [Declaration ann] -> Validation (CheckError ann) ()
doCompleteCheck declarations = do
  () <- checkDeclarations declarations
  ledger <- mapValidationFailure CheckErrorCompileError $ compileDeclarations declarations
  checkLedger ledger

data CheckError ann
  = CheckErrorAccountDeclaredTwice !ann !ann !AccountName
  | CheckErrorUndeclaredAccount !ann !ann !(GenLocated ann AccountName)
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
              show (unAccountName an)
            ]
        )
        [ (toDiagnosePosition adl1, This "This account has been declared here first"),
          (toDiagnosePosition adl2, Where "While trying to check this declaration")
        ]
        []
    CheckErrorUndeclaredAccount tl pl (Located al an) ->
      Err
        (Just "CE_UNDECLARED_ACCOUNT")
        ( unwords
            [ "Account has not been declared:",
              show (unAccountName an)
            ]
        )
        [ (toDiagnosePosition tl, Where "While trying to check this transaction"),
          (toDiagnosePosition al, This "This account has not been declared"),
          (toDiagnosePosition pl, Where "While trying to check this posting")
        ]
        []
    CheckErrorCompileError ce -> toReport ce
    CheckErrorBalanceError be -> toReport be

checkDeclarations :: [Declaration ann] -> Validation (CheckError ann) ()
checkDeclarations ds = do
  checkAccounts ds
  pure ()

checkAccounts :: [Declaration ann] -> Validation (CheckError ann) ()
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

checkAccountsDeclared :: Map AccountName ann -> [Declaration ann] -> Validation (CheckError ann) ()
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

checkLedger :: Ord ann => Ledger ann -> Validation (CheckError ann) ()
checkLedger l = do
  _ <- mapValidationFailure CheckErrorBalanceError $ produceBalanceReport l
  pure ()
