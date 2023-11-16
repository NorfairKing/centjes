{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnboxedTuples #-}

module Centjes.DecimalLiteral
  ( DecimalLiteral (..),
    renderDecimalLiteral,
    parseDecimalLiteral,
    toQuantisationFactor,
    fromQuantisationFactor,
    toAccount,
    fromAccount,
  )
where

import Control.DeepSeq
import Data.List (find)
import Data.Ratio
import Data.Scientific
import Data.Validity
import Data.Validity.Scientific ()
import Data.Word
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor
import Text.ParserCombinators.ReadP (readP_to_S)

newtype DecimalLiteral = DecimalLiteral {unDecimalLiteral :: Scientific}
  deriving (Show, Eq, Ord, Generic)

instance Validity DecimalLiteral where
  validate dl@(DecimalLiteral s) =
    mconcat
      [ genericValidate dl,
        declare "The scientific is small in absolute value" $ base10Exponent s < 128
      ]

instance NFData DecimalLiteral

renderDecimalLiteral :: DecimalLiteral -> String
renderDecimalLiteral = formatScientific Fixed Nothing . unDecimalLiteral

parseDecimalLiteral :: String -> Maybe DecimalLiteral
parseDecimalLiteral = fmap (DecimalLiteral . fst) . find (null . snd) . readP_to_S scientificP

decimalLiteralToRational :: DecimalLiteral -> Rational
decimalLiteralToRational = toRational . unDecimalLiteral -- This is safe because we use small decimal literals

toQuantisationFactor :: DecimalLiteral -> Maybe QuantisationFactor
toQuantisationFactor dl = do
  irat <-
    let r = decimalLiteralToRational dl
     in if numerator r == 0
          then Nothing
          else pure r

  rat <-
    let r = 1 / irat
     in if r < 0
          then Nothing
          else Just r

  fac <-
    if denominator rat == 1
      then Just (numerator rat)
      else Nothing

  if fac <= fromIntegral (maxBound :: Word32)
    then Just (QuantisationFactor (fromIntegral fac))
    else Nothing

fromQuantisationFactor :: QuantisationFactor -> Maybe DecimalLiteral
fromQuantisationFactor (QuantisationFactor qfw) =
  let r = 1 % fromIntegral qfw
   in -- We set a limit for safety reasons.
      case fromRationalRepetend (Just 128) r of
        Left _ -> Nothing
        Right (s, Nothing) -> Just $ DecimalLiteral s
        Right (_, Just _) -> Nothing

toAccount :: QuantisationFactor -> DecimalLiteral -> Maybe Money.Account
toAccount qf dl = Account.fromRational qf $ decimalLiteralToRational dl

fromAccount :: QuantisationFactor -> Money.Account -> Maybe DecimalLiteral
fromAccount qf acc =
  let r = Account.toRational qf acc
   in -- We set a limit for safety reasons.
      case fromRationalRepetend (Just 128) r of
        Left _ -> Nothing
        Right (s, Nothing) -> Just $ DecimalLiteral s
        Right (_, Just _) -> Nothing
