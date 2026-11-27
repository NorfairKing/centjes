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
    SetAsideSettings (..),
  )
where

import Centjes.Switzerland.Report.SetAside.Types (SetAsideInput (..))
import Centjes.Switzerland.Report.Taxes.Types (TaxesInput (..))
import Centjes.Switzerland.Report.VAT.Types (VATInput (..))
import Data.Time
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland (version)
import Text.Colour (TerminalCapabilities)
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromEnv)

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
  { settingsBaseDir :: !(Path Abs Dir),
    settingsLedgerFile :: !(Path Rel File),
    -- We must use a separate file for prices because the swiss taxes require
    -- that we use the prices downloaded with the `download-rates` command.
    settingsPricesFile :: !(Path Rel File),
    settingsWatch :: !Bool,
    settingsClean :: !Bool
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingsBaseDir <-
    choice
      [ directoryPathSetting
          [ help "base directory",
            name "base-dir"
          ],
        runIO getCurrentDir
      ]
  settingsLedgerFile <-
    mapIO parseRelFile $
      setting
        [ help "ledger file",
          reader str,
          short 'l',
          name "ledger",
          value "ledger.cent",
          metavar "FILE_PATH"
        ]
  settingsPricesFile <-
    mapIO parseRelFile $
      setting
        [ help "prices file",
          reader str,
          name "prices",
          value "currency-rates.cent",
          metavar "FILE_PATH"
        ]
  settingsWatch <-
    setting
      [ help "Run centjes in a loop",
        name "watch",
        short 'w',
        metavar "ANY",
        switch True,
        reader exists,
        value False
      ]
  settingsClean <-
    yesNoSwitch
      [ help "Clean the packet directory",
        name "clean",
        value True
      ]
  pure Settings {..}

data Dispatch
  = DispatchTaxes !TaxesSettings
  | DispatchVAT !VATSettings
  | DispatchDownloadRates !DownloadRatesSettings
  | DispatchSetAside !SetAsideSettings

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "taxes" "produce a tax report" $ DispatchTaxes <$> settingsParser,
      command "vat" "produce a VAT report" $ DispatchVAT <$> settingsParser,
      command "download-rates" "Download exchange rates" $ DispatchDownloadRates <$> settingsParser,
      command "set-aside" "compare what is set aside for taxes with what they will cost" $ DispatchSetAside <$> settingsParser
    ]

data TaxesSettings = TaxesSettings
  { taxesSettingsZipFile :: !(Path Abs File),
    taxesSettingsPacketDir :: !(Maybe (Path Abs Dir)),
    taxesSettingsInput :: !TaxesInput
  }

instance HasParser TaxesSettings where
  settingsParser = parseTaxesSettings

{-# ANN parseTaxesSettings ("NOCOVER" :: String) #-}
parseTaxesSettings :: Parser TaxesSettings
parseTaxesSettings = do
  taxesSettingsInput <- settingsParser
  (taxesSettingsZipFile, taxesSettingsPacketDir) <- subConfig_ "taxes" $ do
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
  { vatSettingsZipFile :: !(Path Abs File),
    vatSettingsPacketDir :: !(Maybe (Path Abs Dir)),
    vatSettingsInput :: !VATInput
  }

instance HasParser VATSettings where
  settingsParser = parseVATSettings

{-# ANN parseVATSettings ("NOCOVER" :: String) #-}
parseVATSettings :: Parser VATSettings
parseVATSettings = do
  vatSettingsInput <- settingsParser
  (vatSettingsZipFile, vatSettingsPacketDir) <- subConfig_ "vat" $ do
    zf <-
      filePathSetting
        [ help "path to the zip file to create",
          name "zip-file",
          value "vat-packet.zip"
        ]
    pd <-
      optional $
        directoryPathSetting
          [ help "Path to the packet directory to create",
            name "packet-dir"
          ]
    pure (zf, pd)
  pure VATSettings {..}

data DownloadRatesSettings = DownloadRatesSettings
  { downloadRatesSettingsBegin :: !Day,
    downloadRatesSettingsEnd :: !Day,
    downloadRatesSettingsDestination :: !(Path Abs File)
  }

instance HasParser DownloadRatesSettings where
  settingsParser = parseDownloadRatesSettings

{-# ANN parseDownloadRatesSettings ("NOCOVER" :: String) #-}
parseDownloadRatesSettings :: Parser DownloadRatesSettings
parseDownloadRatesSettings = subConfig_ "download-rates" $ do
  downloadRatesSettingsBegin <-
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
  downloadRatesSettingsEnd <-
    choice
      [ setting
          [ help "The final date (inclusive), default: Yesterday",
            reader auto,
            name "end",
            metavar "YYYY-MM-DD"
          ],
        runIO $ addDays (-1) . utctDay <$> getCurrentTime
      ]
  downloadRatesSettingsDestination <-
    filePathSetting
      [ help "Where to put the resulting file",
        name "output",
        short 'o'
      ]
  pure DownloadRatesSettings {..}

data SetAsideSettings = SetAsideSettings
  { setAsideSettingsInput :: !SetAsideInput,
    -- | The settings of the taxes report, which set-aside produces for its
    -- revenue, business expenses and third pillar contributions.
    setAsideSettingsTaxesInput :: !TaxesInput,
    setAsideSettingsTerminalCapabilities :: !TerminalCapabilities
  }

instance HasParser SetAsideSettings where
  settingsParser = parseSetAsideSettings

{-# ANN parseSetAsideSettings ("NOCOVER" :: String) #-}
parseSetAsideSettings :: Parser SetAsideSettings
parseSetAsideSettings = do
  setAsideSettingsInput <- settingsParser
  setAsideSettingsTaxesInput <- settingsParser
  setAsideSettingsTerminalCapabilities <- runIO getTerminalCapabilitiesFromEnv
  pure SetAsideSettings {..}
