{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Switzerland.Report.VATRate
  ( VATRate (..),
    vatRateRatio,
    parseVATRate,
  )
where

import Data.Ratio
import Data.Validity
import GHC.Generics (Generic)
import Numeric.Natural

data VATRate
  = -- | 7.7%
    VATRate2023Standard
  | -- | 8.1%
    VATRate2024Standard
  | -- | 2.5%
    VATRate2023Reduced
  | -- | 2.6%
    VATRate2024Reduced
  | -- | 3.7%
    VATRate2023Hotel
  | -- | 3.8%
    VATRate2024Hotel
  deriving (Show, Eq, Generic)

instance Validity VATRate

-- TODO move this into a shared module
vatRateRatio :: VATRate -> Ratio Natural
vatRateRatio = \case
  VATRate2023Standard -> 0.077
  VATRate2024Standard -> 0.081
  VATRate2023Reduced -> 0.025
  VATRate2024Reduced -> 0.026
  VATRate2023Hotel -> 0.037
  VATRate2024Hotel -> 0.038

parseVATRate :: Ratio Natural -> Maybe VATRate
parseVATRate = \case
  0.077 -> pure VATRate2023Standard
  0.081 -> pure VATRate2024Standard
  0.025 -> pure VATRate2023Reduced
  0.026 -> pure VATRate2024Reduced
  0.037 -> pure VATRate2023Hotel
  0.038 -> pure VATRate2024Hotel
  _ -> Nothing
