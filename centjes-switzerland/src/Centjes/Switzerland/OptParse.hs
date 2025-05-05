{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.OptParse
  ( getInstructions,
    Instructions (..),
    Settings (..),
    Dispatch (..),
    VATSettings (..),
    TaxesSettings (..),
    DownloadRatesSettings (..),
  )
where

import Centjes.Switzerland.Report.Taxes.Types (TaxesInput (..))
import Centjes.Switzerland.Report.VAT.Types (VATInput (..))
import Data.Time
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland (version)

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "generate reports for swiss taxes"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "centjes-switzerland" $
    withConfigurableYamlConfig (runIO $ resolveFile' "switzerland.yaml") $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Settings = Settings
  { settingBaseDir :: !(Path Abs Dir),
    settingLedgerFile :: !(Path Rel File)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingBaseDir <-
    choice
      [ directoryPathSetting
          [ help "base directory",
            name "base-dir"
          ],
        runIO getCurrentDir
      ]
  settingLedgerFile <-
    mapIO parseRelFile $
      setting
        [ help "ledger file",
          reader str,
          short 'l',
          name "ledger",
          value "ledger.cent",
          metavar "FILE_PATH"
        ]
  pure Settings {..}

data Dispatch
  = DispatchTaxes !TaxesSettings
  | DispatchVAT !VATSettings
  | DispatchDownloadRates !DownloadRatesSettings

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "taxes" "produce a tax report" $ DispatchTaxes <$> settingsParser,
      command "vat" "produce a VAT report" $ DispatchVAT <$> settingsParser,
      command "download-rates" "Download exchange rates" $ DispatchDownloadRates <$> settingsParser
    ]

data TaxesSettings = TaxesSettings
  { taxesSettingZipFile :: !(Path Abs File),
    taxesSettingPacketDir :: !(Maybe (Path Abs Dir)),
    taxesSettingInput :: !TaxesInput
  }

instance HasParser TaxesSettings where
  settingsParser = parseTaxesSettings

{-# ANN parseTaxesSettings ("NOCOVER" :: String) #-}
parseTaxesSettings :: Parser TaxesSettings
parseTaxesSettings = do
  taxesSettingInput <- settingsParser
  (taxesSettingZipFile, taxesSettingPacketDir) <- subConfig_ "taxes" $ do
    zf <-
      filePathSetting
        [ help "Path to the zip file to create",
          name "zip-file",
          value "tax-packet.zip"
        ]
    pd <-
      optional $
        directoryPathSetting
          [ help "Path to the packet directory to create",
            name "packet-dir"
          ]
    pure (zf, pd)
  pure TaxesSettings {..}

data VATSettings = VATSettings
  { vatSettingZipFile :: !(Path Abs File),
    vatSettingReadmeFile :: !(Path Abs File),
    vatSettingInput :: !VATInput
  }

instance HasParser VATSettings where
  settingsParser = parseVATSettings

{-# ANN parseVATSettings ("NOCOVER" :: String) #-}
parseVATSettings :: Parser VATSettings
parseVATSettings = do
  vatSettingInput <- settingsParser
  (vatSettingZipFile, vatSettingReadmeFile) <- subConfig_ "vat" $ do
    zf <-
      filePathSetting
        [ help "path to the zip file to create",
          name "zip-file",
          value "vat-packet.zip"
        ]
    rf <-
      filePathSetting
        [ help "path to the readme file to create",
          name "readme-file",
          value "README.pdf"
        ]
    pure (zf, rf)
  pure VATSettings {..}

data DownloadRatesSettings = DownloadRatesSettings
  { downloadRatesSettingBegin :: !Day,
    downloadRatesSettingEnd :: !Day,
    downloadRatesSettingDestination :: !(Path Abs File)
  }

instance HasParser DownloadRatesSettings where
  settingsParser = parseDownloadRatesSettings

{-# ANN parseDownloadRatesSettings ("NOCOVER" :: String) #-}
parseDownloadRatesSettings :: Parser DownloadRatesSettings
parseDownloadRatesSettings = subConfig_ "download-rates" $ do
  downloadRatesSettingBegin <-
    choice
      [ setting
          [ help "The begin date (inclusive), default: Start of the year",
            reader auto,
            name "begin",
            metavar
              "YYYY-MM-DD"
          ],
        runIO $ do
          today <- utctDay <$> getCurrentTime
          let (y, _, _) = toGregorian today
          pure $ fromGregorian y 1 1
      ]
  downloadRatesSettingEnd <-
    choice
      [ setting
          [ help "The final date (inclusive), default: Yesterday",
            reader auto,
            name "end",
            metavar "YYYY-MM-DD"
          ],
        runIO $ addDays (-1) . utctDay <$> getCurrentTime
      ]
  downloadRatesSettingDestination <-
    filePathSetting
      [ help "Where to put the resulting file",
        name "output",
        short 'o'
      ]
  pure DownloadRatesSettings {..}
