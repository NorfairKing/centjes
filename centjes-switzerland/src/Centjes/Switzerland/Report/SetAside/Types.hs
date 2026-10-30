{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.SetAside.Types
  ( SetAsideInput (..),
    duplicateSetAsideAccounts,
    Taxability (..),
    taxability,
    SetAsideTax (..),
    SetAsideWealth (..),
    parseFraction,
    parseOwnedShares,
    Municipality (..),
    SetAsideReport (..),
    SetAsideLine (..),
    SetAsideRow (..),
    setAsideRowAmount,
    showPercentage,
    WealthTaxBasis (..),
    SetAsidePrices (..),
    SetAsideSurplus (..),
    SetAsideAction (..),
    SetAsideActionKind (..),
    SetAsideError (..),
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Convert
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Tag as Tag
import Centjes.Validation
import Data.Map (Map)
import qualified Data.Map as M
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import Error.Diagnose
import qualified Money.Account as Money (Account)
import qualified Money.Amount as Money (Amount)
import qualified Money.QuantisationFactor as Money (QuantisationFactor)
import OptEnvConf
import Text.Read (readMaybe)

data SetAsideInput = SetAsideInput
  { setAsideInputYear :: !Year,
    -- | The tag of income accounts that are not revenue, like gifts.
    -- It is the taxes report's tag for income that is not declared.
    setAsideInputTagUndeclared :: !Tag,
    setAsideInputSocialSecurity :: !SetAsideTax,
    setAsideInputCantonal :: !SetAsideTax,
    setAsideInputFederal :: !SetAsideTax,
    setAsideInputWealth :: !SetAsideWealth
  }

instance HasParser SetAsideInput where
  settingsParser = checkEither checkSetAsideInput parseSetAsideInput

checkSetAsideInput :: SetAsideInput -> Either String SetAsideInput
checkSetAsideInput input = case duplicateSetAsideAccounts input of
  [] -> Right input
  duplicates ->
    Left $
      unwords $
        "These accounts are in the set-aside settings more than once:"
          : map AccountName.toString duplicates

-- | Reserve accounts that the settings would count twice: one account in two
-- taxes, or twice in one.
duplicateSetAsideAccounts :: SetAsideInput -> [AccountName]
duplicateSetAsideAccounts SetAsideInput {..} =
  let reserveAccounts :: [AccountName]
      reserveAccounts =
        concatMap
          setAsideTaxReserveAccounts
          [setAsideInputSocialSecurity, setAsideInputCantonal, setAsideInputFederal]
          ++ setAsideWealthReserveAccounts setAsideInputWealth
   in M.keys $ M.filter (> 1) $ M.fromListWith (+) [(an, 1 :: Int) | an <- reserveAccounts]

-- | Whether an account counts as taxable wealth, by its tags.
data Taxability ann
  = Taxable
  | NotTaxable
  | -- | Neither tag, which is an error so that no account is left out by
    -- accident.
    Untagged
  | -- | Both tags, at these locations.
    TaggedBoth !ann !ann
  deriving (Show, Eq)

taxability :: SetAsideWealth -> Map Tag ann -> Taxability ann
taxability SetAsideWealth {..} tags =
  case (M.lookup setAsideWealthTagTaxable tags, M.lookup setAsideWealthTagNotTaxable tags) of
    (Just _, Nothing) -> Taxable
    (Nothing, Just _) -> NotTaxable
    (Nothing, Nothing) -> Untagged
    (Just taxableLocation, Just notTaxableLocation) -> TaggedBoth taxableLocation notTaxableLocation

{-# ANN parseSetAsideInput ("NOCOVER" :: String) #-}
parseSetAsideInput :: Parser SetAsideInput
parseSetAsideInput = do
  setAsideInputYear <-
    choice
      [ setting
          [ help "the year to check the set-asides of, default: this year",
            option,
            reader $ maybeReader readMaybe,
            long "year",
            metavar "YYYY"
          ],
        runIO $ (\d -> let (y, _, _) = toGregorian d in y) . utctDay <$> getCurrentTime
      ]
  (setAsideInputTagUndeclared, setAsideInputSocialSecurity, setAsideInputCantonal, setAsideInputFederal, setAsideInputWealth) <-
    subConfig_ "set-aside" $ do
      tagUndeclared <-
        setting
          [ help "tag of income accounts that are not revenue",
            reader $ eitherReader Tag.fromString,
            conf "tag-undeclared",
            value "undeclared"
          ]
      socialSecurity <- subConfig_ "sva" parseSetAsideTax
      cantonal <- subConfig_ "cantonal" parseSetAsideTax
      federal <- subConfig_ "federal" parseSetAsideTax
      wealth <- subConfig_ "wealth" parseSetAsideWealth
      pure (tagUndeclared, socialSecurity, cantonal, federal, wealth)
  pure SetAsideInput {..}

-- | How much is set aside for one tax, and where.
data SetAsideTax = SetAsideTax
  { -- | The share of revenue that is set aside for this tax, between 0 and 1.
    setAsideTaxRate :: !Scientific,
    -- | The accounts that money for this tax is moved or paid into.
    setAsideTaxReserveAccounts :: ![AccountName]
  }

{-# ANN parseSetAsideTax ("NOCOVER" :: String) #-}
parseSetAsideTax :: Parser SetAsideTax
parseSetAsideTax = do
  setAsideTaxRate <-
    checkEither parseFraction $
      setting
        [ help "the share of revenue that is set aside for this tax, between 0 and 1",
          conf "rate",
          example "0.12"
        ]
  setAsideTaxReserveAccounts <-
    setting
      [ help "the accounts that money for this tax is moved or paid into",
        conf "reserves",
        value []
      ]
  pure SetAsideTax {..}

data SetAsideWealth = SetAsideWealth
  { setAsideWealthMunicipality :: !Municipality,
    -- | The tag of asset and liability accounts that count as wealth.
    setAsideWealthTagTaxable :: !Tag,
    -- | The tag of asset and liability accounts that do not, like a third
    -- pillar.
    setAsideWealthTagNotTaxable :: !Tag,
    -- | Accounts that are shared with someone who is taxed separately, and the
    -- share of them that is yours, between 0 and 1.
    -- Other accounts are wholly yours.
    setAsideWealthOwnedShares :: !(Map AccountName Scientific),
    -- | The accounts that money for wealth tax is moved into.
    setAsideWealthReserveAccounts :: ![AccountName]
  }

{-# ANN parseSetAsideWealth ("NOCOVER" :: String) #-}
parseSetAsideWealth :: Parser SetAsideWealth
parseSetAsideWealth = do
  setAsideWealthMunicipality <-
    Municipality
      <$> setting
        [ help "the municipality you live in, by its number from the Federal Statistical Office, for its Steuerfuss",
          conf "municipality",
          -- The city of Zürich.
          value 261
        ]
  setAsideWealthTagTaxable <-
    setting
      [ help "tag of asset and liability accounts that count as wealth",
        reader $ eitherReader Tag.fromString,
        conf "tag-taxable",
        value "taxable"
      ]
  setAsideWealthTagNotTaxable <-
    setting
      [ help "tag of asset and liability accounts that do not count as wealth, like a third pillar",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-taxable",
        value "not-taxable"
      ]
  setAsideWealthOwnedShares <-
    checkMapEither parseOwnedShares $
      setting
        [ help "accounts shared with someone who is taxed separately, and the share of them that is yours, between 0 and 1",
          conf "owned",
          valueWithShown (const "{}") M.empty
        ]
  setAsideWealthReserveAccounts <-
    setting
      [ help "the accounts that money for wealth tax is moved into",
        conf "reserves",
        value []
      ]
  pure SetAsideWealth {..}

parseOwnedShares :: Map Text Scientific -> Either String (Map AccountName Scientific)
parseOwnedShares =
  fmap M.fromList
    . mapM
      ( \(t, share) -> case AccountName.fromText t of
          Nothing -> Left $ unwords ["Invalid account name:", show t]
          Just an -> (,) an <$> parseFraction share
      )
    . M.toList

parseFraction :: Scientific -> Either String Scientific
parseFraction fraction =
  if fraction < 0 || fraction > 1
    then Left $ unwords ["Not between 0 and 1:", show fraction]
    else Right fraction

-- | A municipality in the canton of Zürich, by its number from the Swiss
-- Federal Statistical Office, like 261 for the city of Zürich.
newtype Municipality = Municipality {municipalityNumber :: Int}

-- | The prices to convert with, split by whether the tax administration
-- publishes them.
data SetAsidePrices ann = SetAsidePrices
  { -- | The ESTV's rates, which taxes are computed with: daily exchange
    -- rates and the year-end values of the Kursliste.
    setAsidePricesOfficial :: !(Vector (GenLocated ann (Price ann))),
    -- | Every other price, for what the ESTV does not price (yet).
    setAsidePricesMarket :: !(Vector (GenLocated ann (Price ann)))
  }

-- | All amounts are in CHF.
data SetAsideReport = SetAsideReport
  { setAsideReportYear :: !Year,
    -- | The year's revenue, which the income taxes are set aside from.
    setAsideReportRevenue :: !Money.Account,
    setAsideReportWealthTaxBasis :: !WealthTaxBasis,
    setAsideReportWealthTax :: !SetAsideLine,
    setAsideReportSocialSecurity :: !SetAsideLine,
    setAsideReportCantonal :: !SetAsideLine,
    setAsideReportFederal :: !SetAsideLine,
    setAsideReportActions :: ![SetAsideAction],
    setAsideReportSurpluses :: ![SetAsideSurplus]
  }

-- | What the wealth tax is computed from.
data WealthTaxBasis = WealthTaxBasis
  { -- | The day the wealth is valued on: the end of the year, or today while
    -- the year runs.
    wealthTaxBasisDay :: !Day,
    wealthTaxBasisTaxableWealth :: !Money.Account,
    -- | The einfache Staatssteuer, which each Steuerfuss is a percentage of.
    wealthTaxBasisSimpleTax :: !Money.Account
  }

-- | The rows of one tax, which add up to its surplus: negative when less is
-- set aside than the tax will cost.
data SetAsideLine = SetAsideLine
  { setAsideLineRows :: ![SetAsideRow],
    setAsideLineSurplus :: !Money.Account
  }

data SetAsideRow
  = -- | The share of revenue that a tax costs, negative, and that share in
    -- percent, named after the liability that the tax will become.
    SetAsideRowRevenueShare !AccountName !Scientific !Money.Account
  | -- | The canton's part of the wealth tax, negative, and its Steuerfuss in
    -- percent.
    SetAsideRowCantonalWealthTax !Integer !Money.Account
  | -- | The municipality's part of the wealth tax, negative, and its
    -- Steuerfuss in percent.
    SetAsideRowMunicipalWealthTax !Integer !Money.Account
  | -- | The balance of a reserve account on the valuation day.
    SetAsideRowReserve !AccountName !Money.Account

setAsideRowAmount :: SetAsideRow -> Money.Account
setAsideRowAmount = \case
  SetAsideRowRevenueShare _ _ amount -> amount
  SetAsideRowCantonalWealthTax _ amount -> amount
  SetAsideRowMunicipalWealthTax _ amount -> amount
  SetAsideRowReserve _ amount -> amount

-- | A fraction as a percentage, as people write it: 12 rather than 12.0.
showPercentage :: Scientific -> String
showPercentage fraction =
  let percentage = fraction * 100
   in case Scientific.floatingOrInteger percentage :: Either Double Integer of
        Right whole -> show whole
        Left _ -> Scientific.formatScientific Scientific.Fixed Nothing percentage

-- | What to do about a tax that does not have enough set aside yet.
data SetAsideAction = SetAsideAction
  { setAsideActionKind :: !SetAsideActionKind,
    setAsideActionTax :: !Text,
    -- | Where to put the money, if the settings say.
    setAsideActionAccount :: !(Maybe AccountName),
    setAsideActionShortfall :: !Money.Amount,
    -- | The shortfall rounded up to whole hundreds of CHF, to transfer.
    setAsideActionAmount :: !Money.Amount
  }

data SetAsideActionKind
  = -- | Move money aside, for a tax that cannot be paid before it is billed.
    SetAsideActionSave
  | -- | Pay ahead of the bill.
    SetAsideActionPrepay

-- | A tax that has more set aside than it will cost.
data SetAsideSurplus = SetAsideSurplus
  { setAsideSurplusTax :: !Text,
    setAsideSurplusAmount :: !Money.Amount
  }

data SetAsideError ann
  = SetAsideErrorNoCHF
  | SetAsideErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | SetAsideErrorConvert !(ConvertError ann)
  | SetAsideErrorNoOfficialRateBefore !ann !Day
  | SetAsideErrorNoPrice !CurrencySymbol !Day
  | SetAsideErrorUndeclaredAccount !AccountName
  | SetAsideErrorUntaggedWealth !ann !AccountName
  | SetAsideErrorTaggedBoth !AccountName !ann !ann
  | -- | The wealth tariff and the cantonal Steuerfuss, which change together.
    SetAsideErrorNoCantonalRates !Year
  | SetAsideErrorNoMunicipalSteuerfuss !Municipality !Year
  | SetAsideErrorOverflow

instance ToReport (SetAsideError SourceSpan) where
  toReport = \case
    SetAsideErrorNoCHF -> Err Nothing "No CHF currency declared" [] [Hint "Declare it with: currency CHF 0.01"]
    SetAsideErrorWrongCHF (Located l _) ->
      Err
        Nothing
        "CHF is declared with an unexpected minimal quantisation"
        [(toDiagnosePosition l, This "declared here")]
        [Hint "CHF should be declared with 0.01"]
    SetAsideErrorConvert e -> toReport e
    SetAsideErrorNoOfficialRateBefore l day ->
      Err
        Nothing
        (unwords ["No ESTV rate to convert this posting to CHF on or before", show day])
        [(toDiagnosePosition l, This "this posting")]
        [Hint "Download the rates with centjes-switzerland download-rates."]
    SetAsideErrorNoPrice symbol day ->
      Err
        Nothing
        (unwords ["No price for", show (currencySymbolText symbol), "on or before", show day])
        []
        [Hint (unwords ["Add a price for", T.unpack (currencySymbolText symbol), "on or before that day, in CHF or in a currency with a price in CHF."])]
    SetAsideErrorUndeclaredAccount an ->
      Err
        Nothing
        (unwords ["The set-aside settings mention an account that the ledger does not declare:", AccountName.toString an])
        []
        [Hint "Declare it in the ledger, or remove it from the set-aside settings."]
    SetAsideErrorUntaggedWealth l an ->
      Err
        Nothing
        (unwords ["This account has a balance but is not tagged as taxable wealth or not:", AccountName.toString an])
        [(toDiagnosePosition l, This "declared here")]
        [Hint "Tag it with the tag-taxable or the tag-not-taxable tag of the set-aside wealth settings."]
    SetAsideErrorTaggedBoth an taxableLocation notTaxableLocation ->
      Err
        Nothing
        (unwords ["This account is tagged both as taxable wealth and as not:", AccountName.toString an])
        [ (toDiagnosePosition taxableLocation, This "taxable here"),
          (toDiagnosePosition notTaxableLocation, This "not taxable here")
        ]
        [Hint "Remove one of the two tags."]
    SetAsideErrorNoCantonalRates y ->
      Err
        Nothing
        (unwords ["No Zürich wealth tax tariff and cantonal Steuerfuss are known for", show y])
        []
        [Hint "Add them to zurichMarriedWealthTariff and zurichCantonalSteuerfuss in centjes-switzerland."]
    SetAsideErrorNoMunicipalSteuerfuss (Municipality m) y ->
      Err
        Nothing
        (unwords ["No Steuerfuss is known for municipality", show m, "in", show y])
        []
        [Hint "Add it to zurichMunicipalSteuerfuss in centjes-switzerland."]
    SetAsideErrorOverflow -> Err Nothing "Amounts too large to add up" [] []
