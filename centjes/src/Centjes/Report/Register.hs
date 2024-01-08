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
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Control.DeepSeq
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
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

convertRegister ::
  forall ann.
  Ord ann =>
  MemoisedPriceGraph (Currency ann) ->
  Currency ann ->
  Register ann ->
  Validation (ConvertError ann) (Register ann)
convertRegister mpg currencyTo = fmap Register . traverse goT . registerTransactions
  where
    goT ::
      ( GenLocated ann Timestamp,
        Maybe (GenLocated ann Description),
        Vector
          ( GenLocated ann (Posting ann),
            Money.MultiAccount (Currency ann)
          )
      ) ->
      Validation
        (ConvertError ann)
        ( GenLocated ann Timestamp,
          Maybe (GenLocated ann Description),
          Vector
            ( GenLocated ann (Posting ann),
              Money.MultiAccount (Currency ann)
            )
        )
    goT (lt, mld, vps) = (,,) lt mld <$> traverse goP vps
    goP ::
      ( GenLocated ann (Posting ann),
        Money.MultiAccount (Currency ann)
      ) ->
      Validation
        (ConvertError ann)
        ( GenLocated ann (Posting ann),
          Money.MultiAccount (Currency ann)
        )
    goP (p, ma) = (,) p <$> convertMultiAccount mpg currencyTo ma

produceRegister ::
  forall ann.
  Ord ann =>
  Filter ->
  Maybe CurrencySymbol ->
  Ledger ann ->
  Validation (RegisterError ann) (Register ann)
produceRegister f mCurrencySymbolTo ledger = do
  ts <- mapM (registerTransaction f) (ledgerTransactions ledger)
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

  let r = Register ts'

  mapValidationFailure RegisterErrorConvertError $ case mCurrencySymbolTo of
    Nothing -> pure r
    Just currencySymbolTo -> do
      currencyTo <- lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      convertRegister
        (pricesToPriceGraph (ledgerPrices ledger))
        currencyTo
        r

registerTransaction ::
  Filter ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    ( GenLocated ann Timestamp,
      Maybe (GenLocated ann Description),
      Vector (GenLocated ann (Posting ann))
    )
registerTransaction f (Located _ t) = do
  let postings =
        V.filter
          ( \(Located _ Posting {..}) ->
              let Located _ an = postingAccountName
               in Filter.predicate f an
          )
          (transactionPostings t)
  pure
    ( transactionTimestamp t,
      transactionDescription t,
      postings
    )
