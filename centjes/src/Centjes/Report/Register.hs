{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import GHC.Generics (Generic)
import qualified Money.Account as Account

newtype Register ann = Register {registerTransactions :: Vector (GenLocated ann (Transaction ann))}
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Register ann)

instance NFData ann => NFData (Register ann)

data RegisterError ann
  = RegisterErrorConvertError !(ConvertError ann)
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance NFData ann => NFData (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterErrorConvertError ce -> toReport ce

produceRegister :: Maybe CurrencySymbol -> Ledger ann -> Validation (RegisterError ann) (Register ann)
produceRegister mCurrencySymbolTo ledger = do
  mCurrencyTo <-
    mapM
      ( mapValidationFailure RegisterErrorConvertError
          . lookupConversionCurrency (ledgerCurrencies ledger)
      )
      mCurrencySymbolTo

  Register <$> mapM (registerTransaction (ledgerPrices ledger) mCurrencyTo) (ledgerTransactions ledger)

registerTransaction ::
  Vector (GenLocated ann (Price ann)) ->
  Maybe (Currency ann) ->
  GenLocated ann (Transaction ann) ->
  Validation (RegisterError ann) (GenLocated ann (Transaction ann))
registerTransaction prices mCurrencyTo (Located l t) = do
  ps' <- traverse (registerPosting prices mCurrencyTo) (transactionPostings t)
  pure (Located l (t {transactionPostings = ps'}))

registerPosting ::
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
