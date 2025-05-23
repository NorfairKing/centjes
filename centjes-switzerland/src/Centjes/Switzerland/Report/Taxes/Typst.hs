{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes.Typst
  ( Input (..),
    taxesReportInput,
  )
where

import Autodocodec
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.Taxes.Types
import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Text (Text)
import Data.Time
import qualified Money.Account as Account
import qualified Money.ConversionRate as ConversionRate
import Numeric.Natural
import Path
import Text.Printf

taxesReportInput :: TaxesReport ann -> Input
taxesReportInput TaxesReport {..} =
  let formatAccount cur = Account.format (locatedValue (currencyQuantisationFactor cur))
      formatChfAccount = formatAccount taxesReportCHF

      inputLastName = taxesReportLastName
      inputFirstName = taxesReportFirstName
      inputYear = taxesReportYear
      inputConversionRates =
        M.fromList
          $ map
            ( \(Currency {..}, rate) ->
                ( CurrencySymbol.toText currencySymbol,
                  printf "%.4f" $ (realToFrac :: Ratio Natural -> Double) $ ConversionRate.toRatio rate
                )
            )
          $ M.toList taxesReportConversionRates
      inputAssets =
        map
          ( \AssetAccount {..} ->
              let assetInputAccountName = assetAccountName
                  assetInputBalances =
                    M.fromList $
                      map
                        ( \(cur@Currency {..}, (account, chfAccount)) ->
                            ( CurrencySymbol.toText currencySymbol,
                              Balance {balanceOriginal = formatAccount cur account, balanceConverted = formatChfAccount chfAccount}
                            )
                        )
                        (M.toList assetAccountBalances)
                  assetInputConvertedBalance = formatChfAccount assetAccountConvertedBalance
                  assetInputEvidence = assetAccountAttachments
               in AssetInput {..}
          )
          taxesReportAssetAccounts
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputLastName :: Text,
    inputFirstName :: Text,
    inputYear :: !Year,
    inputConversionRates :: !(Map Text String),
    inputAssets :: ![AssetInput]
  }
  deriving (ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "last_name" "last name"
          .= inputLastName
        <*> requiredField "first_name" "first name"
          .= inputFirstName
        <*> requiredField "year" "year"
          .= inputYear
        <*> requiredField "rates" "conversion rates"
          .= inputConversionRates
        <*> requiredField "assets" "assets"
          .= inputAssets

data AssetInput = AssetInput
  { assetInputAccountName :: !AccountName,
    assetInputBalances :: !(Map Text Balance),
    assetInputConvertedBalance :: !String,
    assetInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec AssetInput where
  codec =
    object "AssetInput" $
      AssetInput
        <$> requiredField "name" "name"
          .= assetInputAccountName
        <*> requiredField "balances" "balances"
          .= assetInputBalances
        <*> requiredField "balance" "balance"
          .= assetInputConvertedBalance
        <*> requiredField "evidence" "evidence"
          .= assetInputEvidence

data Balance = Balance
  { balanceOriginal :: String,
    balanceConverted :: String
  }

instance HasCodec Balance where
  codec =
    object "Balance" $
      Balance
        <$> requiredField "original" "balance in original currency" .= balanceOriginal
        <*> requiredField "converted" "balance in CHF" .= balanceConverted
