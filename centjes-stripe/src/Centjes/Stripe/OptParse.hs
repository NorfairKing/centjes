{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Stripe.OptParse
  ( getSettings,
    Settings (..),
    Command (..),
    ImportSettings (..),
    importDeclarationSettings,
  )
where

import qualified Centjes.AccountName as AccountName
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Stripe.API (StripeKey (..))
import Centjes.Stripe.Declarations (DeclarationSettings (..))
import Centjes.Switzerland.Report.VAT.Types (VATRate (..), vatRatePercentageNumber)
import qualified Centjes.Tag as Tag
import Centjes.Validation (renderInvalid)
import Data.Bifunctor (first)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Calendar.Month
import OptEnvConf hiding (Command)
import Path
import Path.IO
import Paths_centjes_stripe (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "Stripe importer for centjes"

data Settings = Settings
  { settingsLedgerFile :: !(Path Abs File),
    settingsCommand :: !Command
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-stripe" $
    withConfigurableYamlConfig (runIO $ resolveFile' "stripe.yaml") $ do
      settingsLedgerFile <-
        filePathSetting
          [ help "ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent",
            metavar "FILE_PATH"
          ]
      settingsCommand <- settingsCommandParser
      pure Settings {..}

newtype Command
  = CommandImport ImportSettings

settingsCommandParser :: Parser Command
settingsCommandParser =
  commands
    [ command "import" "Import Stripe sales, fees and payouts month by month" $
        CommandImport <$> parseImportSettings
    ]

data ImportSettings = ImportSettings
  { importSettingsKey :: !StripeKey,
    -- | The file this importer owns and rewrites.
    importSettingsOutput :: !(Path Abs File),
    -- | The first month to import.
    --
    -- Months before this one still count towards the balance, and their total
    -- becomes an opening balance transaction, so that starting part-way through
    -- an account's life still produces assertions that hold.
    importSettingsBegin :: !Month,
    importSettingsAssetsAccountName :: !AccountName.AccountName,
    importSettingsDomesticIncomeAccountName :: !AccountName.AccountName,
    importSettingsVATIncomeAccountName :: !AccountName.AccountName,
    importSettingsForeignIncomeAccountName :: !AccountName.AccountName,
    importSettingsFeesAccountName :: !AccountName.AccountName,
    importSettingsRefundsAccountName :: !AccountName.AccountName,
    importSettingsVATExpensesAccountName :: !AccountName.AccountName,
    importSettingsPayoutAccountName :: !AccountName.AccountName,
    importSettingsOpeningAccountName :: !AccountName.AccountName,
    importSettingsDeductibleTags :: ![Tag.Tag],
    importSettingsNotVATDeductibleTags :: ![Tag.Tag],
    importSettingsHomeCountry :: !Text,
    -- | The rate Stripe charges VAT on its own fees at, as a percentage.
    importSettingsFeesVATRatePercentage :: !Rational,
    -- | The currency Stripe settles in, which is the one the reports are read in.
    importSettingsCurrency :: !CurrencySymbol.CurrencySymbol,
    importSettingsDocumentsDirectory :: !(Path Rel Dir),
    -- | Where to save the report CSVs, relative to the ledger.
    importSettingsReportsDirectory :: !(Path Rel Dir),
    importSettingsFeesAttachmentPattern :: !String
  }

-- | The part of the import settings the emitter needs.
importDeclarationSettings :: ImportSettings -> DeclarationSettings
importDeclarationSettings ImportSettings {..} =
  DeclarationSettings
    { declarationSettingsAssetsAccountName = importSettingsAssetsAccountName,
      declarationSettingsDomesticIncomeAccountName = importSettingsDomesticIncomeAccountName,
      declarationSettingsVATIncomeAccountName = importSettingsVATIncomeAccountName,
      declarationSettingsForeignIncomeAccountName = importSettingsForeignIncomeAccountName,
      declarationSettingsFeesAccountName = importSettingsFeesAccountName,
      declarationSettingsRefundsAccountName = importSettingsRefundsAccountName,
      declarationSettingsVATExpensesAccountName = importSettingsVATExpensesAccountName,
      declarationSettingsPayoutAccountName = importSettingsPayoutAccountName,
      declarationSettingsOpeningAccountName = importSettingsOpeningAccountName,
      declarationSettingsDeductibleTags = importSettingsDeductibleTags,
      declarationSettingsNotVATDeductibleTags = importSettingsNotVATDeductibleTags,
      declarationSettingsDocumentsDirectory = importSettingsDocumentsDirectory,
      declarationSettingsReportsDirectory = importSettingsReportsDirectory,
      declarationSettingsFeesAttachmentPattern = importSettingsFeesAttachmentPattern
    }

{-# ANN parseImportSettings ("NOCOVER" :: String) #-}
parseImportSettings :: Parser ImportSettings
parseImportSettings = subConfig_ "import" $ do
  importSettingsKey <-
    StripeKey
      <$> secretTextFileOrBareSetting
        [ help "Stripe API key that can read the balance and payouts and create report runs",
          name "key"
        ]
  importSettingsOutput <-
    filePathSetting
      [ help "Output file path, which this importer rewrites",
        short 'o',
        name "output",
        value "stripe.cent",
        metavar "FILE_PATH"
      ]
  importSettingsBegin <-
    choice
      [ checkMapEither
          ( \s -> case parseTimeM True defaultTimeLocale "%Y-%m" s of
              Nothing -> Left $ unwords ["Not a month:", show (s :: String)]
              Just month -> Right month
          )
          $ setting
            [ help "The first month to import, default: January of this year",
              reader str,
              name "begin",
              metavar "YYYY-MM"
            ],
        runIO $ do
          today <- utctDay <$> getCurrentTime
          let (y, _, _) = toGregorian today
          pure $ YearMonth y 1
      ]
  importSettingsAssetsAccountName <-
    setting
      [ help "Account name of the Stripe balance",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "assets-account",
        value "assets:stripe"
      ]
  importSettingsDomesticIncomeAccountName <-
    setting
      [ help "Account name of domestic income",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "domestic-income-account",
        value "income:domestic"
      ]
  importSettingsVATIncomeAccountName <-
    setting
      [ help "Account name of the VAT charged to customers",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "vat-income-account",
        value "income:VAT"
      ]
  importSettingsForeignIncomeAccountName <-
    setting
      [ help "Account name of foreign income",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "foreign-income-account",
        value "income:foreign"
      ]
  importSettingsFeesAccountName <-
    setting
      [ help "Account name of Stripe's fees",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "fees-account",
        value "expenses:banking:stripe"
      ]
  importSettingsRefundsAccountName <-
    setting
      [ help "Account name of money given back to customers",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "refunds-account",
        value "expenses:refunds"
      ]
  importSettingsVATExpensesAccountName <-
    setting
      [ help "Account name of the VAT paid, which is what gets reclaimed",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "vat-expenses-account",
        value "expenses:VAT"
      ]
  importSettingsPayoutAccountName <-
    setting
      [ help "Account name of the self-transfer that payouts go through",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "payout-account",
        value "assets:self-transfer:stripe-neon"
      ]
  importSettingsOpeningAccountName <-
    setting
      [ help "Account name that the opening balance comes from",
        reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
        conf "opening-account",
        value "equity:starting"
      ]
  importSettingsDeductibleTags <-
    setting
      [ help "Tags for an expense transaction whose expenses carried recoverable VAT",
        conf "deductible-tags",
        value ["deductible"]
      ]
  importSettingsNotVATDeductibleTags <-
    setting
      [ help "Tags for an expense transaction whose expenses carried no VAT",
        conf "not-vat-deductible-tags",
        value ["tax-deductible", "not-vat-deductible"]
      ]
  importSettingsHomeCountry <-
    setting
      [ help "The two-letter country whose VAT this ledger owes",
        reader str,
        conf "home-country",
        value "CH",
        metavar "COUNTRY"
      ]
  importSettingsFeesVATRatePercentage <-
    -- Through 'Scientific' rather than a floating point number, so that the rate written
    -- down is the rate used: 8.1 percent is 81/10 exactly and not nearly that.
    toRational
      <$> setting
        [ help "The percentage Stripe charges VAT on its own fees at",
          reader (auto :: Reader Scientific),
          conf "fees-vat-rate",
          value (vatRatePercentageNumber VATRate2024Standard),
          metavar "PERCENTAGE"
        ]
  importSettingsCurrency <-
    setting
      [ help "The currency Stripe settles in",
        reader $ eitherReader (first renderInvalid . CurrencySymbol.fromText . Text.pack),
        conf "currency",
        value (CurrencySymbol.CurrencySymbol "CHF"),
        metavar "CURRENCY"
      ]
  importSettingsDocumentsDirectory <-
    checkMapEither
      ( \s -> case parseRelDir s of
          Nothing -> Left $ unwords ["Not a relative directory path:", show s]
          Just relDir -> Right relDir
      )
      $ setting
        [ help "Directory of the Stripe documents downloaded by hand, relative to the ledger",
          reader str,
          name "documents",
          value "documents",
          metavar "DIRECTORY_PATH"
        ]
  importSettingsReportsDirectory <-
    checkMapEither
      ( \s -> case parseRelDir s of
          Nothing -> Left $ unwords ["Not a relative directory path:", show s]
          Just relDir -> Right relDir
      )
      $ setting
        [ help "Directory to save the monthly report CSVs into, relative to the ledger",
          reader str,
          name "reports",
          value "documents",
          metavar "DIRECTORY_PATH"
        ]
  importSettingsFeesAttachmentPattern <-
    setting
      [ help "formatTime pattern for Stripe's monthly tax invoice, the one document no API serves",
        reader str,
        conf "fees-attachment",
        value "tax-invoice-%Y-%m.pdf",
        metavar "PATTERN"
      ]
  pure ImportSettings {..}
