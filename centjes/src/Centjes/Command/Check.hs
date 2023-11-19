{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Check
  ( runCentjesCheck,
    CheckError (..),
    checkDeclarations,
    checkAccounts,
    checkAccountsUnique,
    checkAccountsDeclared,
  )
where

import Centjes.Load
import Centjes.Module
import Centjes.OptParse
import Centjes.Validation
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity (Validity)
import GHC.Generics (Generic)
import System.Exit

runCentjesCheck :: Settings -> CheckSettings -> IO ()
runCentjesCheck Settings {..} CheckSettings = runStderrLoggingT $ do
  declarations <- loadModules settingLedgerFile
  case checkDeclarations declarations of
    Success () -> pure ()
    Failure errs -> liftIO $ die $ unlines $ map displayException (NE.toList errs)

data CheckError
  = CheckErrorAccountDeclaredTwice !AccountName
  | CheckErrorUndeclaredAccount !AccountName
  deriving (Show, Eq, Generic)

instance Validity CheckError

instance NFData CheckError

instance Exception CheckError where
  displayException = \case
    CheckErrorAccountDeclaredTwice an ->
      unwords
        [ "Account has been declared twice:",
          show (unAccountName an)
        ]
    CheckErrorUndeclaredAccount an ->
      unwords
        [ "Account has not been declared:",
          show (unAccountName an)
        ]

checkDeclarations :: [Declaration] -> Validation CheckError ()
checkDeclarations ds = do
  checkAccounts ds
  pure ()

checkAccounts :: [Declaration] -> Validation CheckError ()
checkAccounts ds = do
  as <- checkAccountsUnique ds
  checkAccountsDeclared as ds

checkAccountsUnique :: [Declaration] -> Validation CheckError (Set AccountName)
checkAccountsUnique ds =
  validateUniquesSet CheckErrorAccountDeclaredTwice $
    mapMaybe
      ( \case
          DeclarationAccount AccountDeclaration {..} -> Just accountDeclarationName
          _ -> Nothing
      )
      ds

checkAccountsDeclared :: Set AccountName -> [Declaration] -> Validation CheckError ()
checkAccountsDeclared as = traverse_ $ \case
  DeclarationCurrency _ -> pure ()
  DeclarationAccount _ -> pure ()
  DeclarationTransaction t -> for_ (transactionPostings t) $ \p ->
    let an = postingAccountName p
     in if S.member an as
          then pure ()
          else validationFailure $ CheckErrorUndeclaredAccount an

validateUniquesSet ::
  forall a e.
  Ord a =>
  (a -> e) ->
  [a] ->
  Validation e (Set a)
validateUniquesSet errFunc = foldM go S.empty
  where
    go :: Set a -> a -> Validation e (Set a)
    go s a =
      if S.member a s
        then validationFailure (errFunc a)
        else pure $ S.insert a s
