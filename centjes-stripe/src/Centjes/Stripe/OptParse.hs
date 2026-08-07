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
  { settingLedgerFile :: !(Path Abs File),
    settingCommand :: !Command
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-stripe" $
    withConfigurableYamlConfig (runIO $ resolveFile' "stripe.yaml") $ do
      settingLedgerFile <-
        filePathSetting
          [ help "ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent",
            metavar "FILE_PATH"
          ]
      settingCommand <- settingsCommandParser
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
  { importSettingKey :: !StripeKey,
    -- | The file this importer owns and rewrites.
    importSettingOutput :: !(Path Abs File),
    -- | The first month to import.
    --
    -- Months before this one still count towards the balance, and their total
    -- becomes an opening balance transaction, so that starting part-way through
    -- an account's life still produces assertions that hold.
    importSettingBegin :: !Month,
    importSettingAssetsAccountName :: !AccountName.AccountName,
    importSettingDomesticIncomeAccountName :: !AccountName.AccountName,
    importSettingVATIncomeAccountName :: !AccountName.AccountName,
    importSettingForeignIncomeAccountName :: !AccountName.AccountName,
    importSettingFeesAccountName :: !AccountName.AccountName,
    importSettingRefundsAccountName :: !AccountName.AccountName,
    importSettingVATExpensesAccountName :: !AccountName.AccountName,
    importSettingPayoutAccountName :: !AccountName.AccountName,
    importSettingOpeningAccountName :: !AccountName.AccountName,
    importSettingDeductibleTags :: ![Tag.Tag],
    importSettingNotVATDeductibleTags :: ![Tag.Tag],
    importSettingHomeCountry :: !Text,
    -- | The rate Stripe charges VAT on its own fees at, as a percentage.
    importSettingFeesVATRatePercentage :: !Rational,
    -- | The currency Stripe settles in, which is the one the reports are read in.
    importSettingCurrency :: !CurrencySymbol.CurrencySymbol,
    importSettingDocumentsDirectory :: !(Path Rel Dir),
    -- | Where to save the report CSVs, relative to the ledger.
    importSettingReportsDirectory :: !(Path Rel Dir),
    importSettingFeesAttachmentPattern :: !String
  }

-- | The part of the import settings the emitter needs.
importDeclarationSettings :: ImportSettings -> DeclarationSettings
importDeclarationSettings ImportSettings {..} =
  DeclarationSettings
    { declarationSettingAssetsAccountName = importSettingAssetsAccountName,
      declarationSettingDomesticIncomeAccountName = importSettingDomesticIncomeAccountName,
      declarationSettingVATIncomeAccountName = importSettingVATIncomeAccountName,
      declarationSettingForeignIncomeAccountName = importSettingForeignIncomeAccountName,
      declarationSettingFeesAccountName = importSettingFeesAccountName,
      declarationSettingRefundsAccountName = importSettingRefundsAccountName,
      declarationSettingVATExpensesAccountName = importSettingVATExpensesAccountName,
      declarationSettingPayoutAccountName = importSettingPayoutAccountName,
      declarationSettingOpeningAccountName = importSettingOpeningAccountName,
      declarationSettingDeductibleTags = importSettingDeductibleTags,
      declarationSettingNotVATDeductibleTags = importSettingNotVATDeductibleTags,
      declarationSettingDocumentsDirectory = importSettingDocumentsDirectory,
      declarationSettingReportsDirectory = importSettingReportsDirectory,
      declarationSettingFeesAttachmentPattern = importSettingFeesAttachmentPattern
    }

{-# ANN parseImportSettings ("NOCOVER" :: String) #-}
parseImportSettings :: Parser ImportSettings
parseImportSettings = subConfig_ "import" $ do
  importSettingKey <-
    StripeKey
      <$> secretTextFileOrBareSetting
        [ help "Stripe API key that can read the balance and payouts and create report runs",
          name "key"
        ]
  importSettingOutput <-
    filePathSetting
      [ help "Output file path, which this importer rewrites",
        short 'o',
        name "output",
        value "stripe.cent",
        metavar "FILE_PATH"
      ]
  importSettingBegin <-
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
  importSettingAssetsAccountName <-
    setting
      [ help "Account name of the Stripe balance",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "assets-account",
        value "assets:stripe"
      ]
  importSettingDomesticIncomeAccountName <-
    setting
      [ help "Account name of domestic income",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "domestic-income-account",
        value "income:domestic"
      ]
  importSettingVATIncomeAccountName <-
    setting
      [ help "Account name of the VAT charged to customers",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "vat-income-account",
        value "income:VAT"
      ]
  importSettingForeignIncomeAccountName <-
    setting
      [ help "Account name of foreign income",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "foreign-income-account",
        value "income:foreign"
      ]
  importSettingFeesAccountName <-
    setting
      [ help "Account name of Stripe's fees",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "fees-account",
        value "expenses:banking:stripe"
      ]
  importSettingRefundsAccountName <-
    setting
      [ help "Account name of money given back to customers",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "refunds-account",
        value "expenses:refunds"
      ]
  importSettingVATExpensesAccountName <-
    setting
      [ help "Account name of the VAT paid, which is what gets reclaimed",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "vat-expenses-account",
        value "expenses:VAT"
      ]
  importSettingPayoutAccountName <-
    setting
      [ help "Account name of the self-transfer that payouts go through",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "payout-account",
        value "assets:self-transfer:stripe-neon"
      ]
  importSettingOpeningAccountName <-
    setting
      [ help "Account name that the opening balance comes from",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "opening-account",
        value "equity:starting"
      ]
  importSettingDeductibleTags <-
    setting
      [ help "Tags for an expense transaction whose expenses carried recoverable VAT",
        conf "deductible-tags",
        value ["deductible"]
      ]
  importSettingNotVATDeductibleTags <-
    setting
      [ help "Tags for an expense transaction whose expenses carried no VAT",
        conf "not-vat-deductible-tags",
        value ["tax-deductible", "not-vat-deductible"]
      ]
  importSettingHomeCountry <-
    setting
      [ help "The two-letter country whose VAT this ledger owes",
        reader str,
        conf "home-country",
        value "CH",
        metavar "COUNTRY"
      ]
  importSettingFeesVATRatePercentage <-
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
  importSettingCurrency <-
    setting
      [ help "The currency Stripe settles in",
        reader $ eitherReader (either Left Right . CurrencySymbol.fromText . Text.pack),
        conf "currency",
        value (CurrencySymbol.CurrencySymbol "CHF"),
        metavar "CURRENCY"
      ]
  importSettingDocumentsDirectory <-
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
  importSettingReportsDirectory <-
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
  importSettingFeesAttachmentPattern <-
    setting
      [ help "formatTime pattern for Stripe's monthly tax invoice, the one document no API serves",
        reader str,
        conf "fees-attachment",
        value "tax-invoice-%Y-%m.pdf",
        metavar "PATTERN"
      ]
  pure ImportSettings {..}
