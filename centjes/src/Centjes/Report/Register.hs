{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Register
  ( Register (..),
    RegisterError (..),
    produceRegister,
  )
where

import Centjes.Convert
import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Control.DeepSeq
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

newtype Register ann = Register
  { registerTransactions ::
      Vector
        ( GenLocated ann Timestamp,
          Maybe (GenLocated ann Description),
          Vector
            ( GenLocated ann (Posting ann),
              Money.MultiAccount (Currency ann)
            )
        )
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Register ann)

instance NFData ann => NFData (Register ann)

data RegisterError ann
  = RegisterErrorAddError
  | RegisterErrorConvertError !(ConvertError ann)
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance NFData ann => NFData (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterErrorAddError -> undefined
    RegisterErrorConvertError ce -> toReport ce

produceRegister ::
  forall ann.
  Ord ann =>
  Maybe CurrencySymbol ->
  Ledger ann ->
  Validation (RegisterError ann) (Register ann)
produceRegister mCurrencySymbolTo ledger = do
  mCurrencyTo <-
    mapM
      ( mapValidationFailure RegisterErrorConvertError
          . lookupConversionCurrency (ledgerCurrencies ledger)
      )
      mCurrencySymbolTo

  ts <-
    mapM
      ( registerTransaction
          (ledgerPrices ledger)
          mCurrencyTo
      )
      (ledgerTransactions ledger)
  let goTransaction ::
        (Int, Money.MultiAccount (Currency ann)) ->
        Validation
          (RegisterError ann)
          ( ( GenLocated ann Timestamp,
              Maybe (GenLocated ann Description),
              Vector
                ( GenLocated ann (Posting ann),
                  Money.MultiAccount (Currency ann)
                )
            ),
            (Int, Money.MultiAccount (Currency ann))
          )
      goTransaction (ix, runningTotal) = do
        let (lts, ld, ps) = V.unsafeIndex ts ix
        let goPosting ::
              (Int, Money.MultiAccount (Currency ann)) ->
              Validation
                (RegisterError ann)
                ( ( GenLocated ann (Posting ann),
                    Money.MultiAccount (Currency ann)
                  ),
                  (Int, Money.MultiAccount (Currency ann))
                )
            goPosting (jx, runningSubTotal) = do
              let lp@(Located _ Posting {..}) = V.unsafeIndex ps jx
              let Located _ currency = postingCurrency
              let Located _ account = postingAccount
              newRunningTotal <-
                case MultiAccount.addAccount runningSubTotal currency account of
                  Nothing -> validationFailure RegisterErrorAddError
                  Just nt -> pure nt
              pure
                ( (lp, newRunningTotal),
                  (succ jx, newRunningTotal)
                )
        newPostings <- V.unfoldrExactNM (V.length ps) goPosting (0, runningTotal)
        let newRunningTotal =
              if V.null newPostings
                then runningTotal
                else snd (V.last newPostings)
        pure
          ( (lts, ld, newPostings),
            (succ ix, newRunningTotal)
          )

  ts' <- V.unfoldrExactNM (V.length ts) goTransaction (0, MultiAccount.zero)

  pure $ Register ts'

registerTransaction ::
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  Maybe (Currency ann) ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    ( GenLocated ann Timestamp,
      Maybe (GenLocated ann Description),
      Vector (GenLocated ann (Posting ann))
    )
registerTransaction prices mCurrencyTo (Located _ t) = do
  ps' <-
    traverse
      (registerPosting prices mCurrencyTo)
      (transactionPostings t)
  pure
    ( transactionTimestamp t,
      transactionDescription t,
      ps'
    )

registerPosting ::
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  Maybe (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation (RegisterError ann) (GenLocated ann (Posting ann))
registerPosting prices mCurrencyTo (Located l p) = do
  p' <- case mCurrencyTo of
    Nothing -> pure p
    Just currencyTo -> convertPosting prices currencyTo l p
  pure (Located l p')

convertPosting ::
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  Currency ann ->
  ann ->
  Posting ann ->
  Validation (RegisterError ann) (Posting ann)
convertPosting prices currencyTo l p =
  if currencySymbol (locatedValue (postingCurrency p)) == currencySymbol currencyTo
    then pure p
    else do
      let Located cl currencyFrom = postingCurrency p
      (cr, qfFrom) <-
        mapValidationFailure RegisterErrorConvertError $
          lookupConversionRate prices currencyTo l currencyFrom
      let Located al a = postingAccount p
      let qfTo = locatedValue (currencyQuantisationFactor currencyTo)
      let (mResult, _) = Account.convert Account.RoundNearest qfFrom a cr qfTo
      case mResult of
        Nothing -> validationFailure $ RegisterErrorConvertError $ ConvertErrorInvalidSum currencyTo l
        Just result ->
          pure $
            p
              { postingAccount = Located al result,
                postingCurrency = Located cl currencyTo
              }
