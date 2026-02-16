{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.OptParse where

import Autodocodec
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Report.Register
import Control.Applicative
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Time
import OptEnvConf
import Path
import Path.IO
import Paths_centjes (version)
import Text.Colour
import Text.Colour.Capabilities.FromEnv

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "really safe double-entry accounting"

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show)

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "centjes" $
    withConfigurableYamlConfig (runIO $ resolveFile' "centjes.yaml") $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingWatch :: !Bool,
    settingLogLevel :: !LogLevel,
    settingTerminalCapabilities :: !TerminalCapabilities
  }
  deriving (Show)

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingLedgerFile <-
    filePathSetting
      [ help "ledger file",
        short 'l',
        name "ledger",
        value "ledger.cent"
      ]
  settingWatch <-
    setting
      [ help "Run centjes in a loop",
        name "watch",
        short 'w',
        metavar "ANY",
        switch True,
        reader exists,
        value False
      ]
  settingLogLevel <- settingsParser
  settingTerminalCapabilities <- runIO getTerminalCapabilitiesFromEnv
  pure Settings {..}

instance HasParser LogLevel where
  settingsParser =
    setting
      [ help "Minimal severity of log messages",
        reader logLevelReader,
        value LevelInfo,
        name "log-level",
        metavar "LOG_LEVEL",
        example "Info"
      ]
    where
      logLevelReader = eitherReader $ \case
        "Debug" -> Right LevelDebug
        "Info" -> Right LevelInfo
        "Warn" -> Right LevelWarn
        "Error" -> Right LevelError
        s -> Left $ "Unknown LogLevel: " <> show s

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

data Dispatch
  = DispatchCheck !CheckSettings
  | DispatchRegister !RegisterSettings
  | DispatchBalance !BalanceSettings
  | DispatchFormat !FormatSettings
  | DispatchRatesGraph !RatesGraphSettings
  deriving (Show)

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "check" "perform an internal consistency check" $ DispatchCheck <$> settingsParser,
      command "register" "register report" $ DispatchRegister <$> settingsParser,
      command "balance" "balance report" $ DispatchBalance <$> settingsParser,
      command "format" "format files" $ DispatchFormat <$> settingsParser,
      command "rates-graph" "graph exchange rates" $ DispatchRatesGraph <$> settingsParser
    ]

data CheckSettings = CheckSettings
  deriving (Show)

instance HasParser CheckSettings where
  settingsParser = parseCheckSettings

{-# ANN parseCheckSettings ("NOCOVER" :: String) #-}
parseCheckSettings :: Parser CheckSettings
parseCheckSettings = pure CheckSettings

data RegisterSettings = RegisterSettings
  { registerSettingFilter :: !Filter,
    registerSettingBlockSize :: !BlockSize,
    registerSettingCurrency :: !(Maybe CurrencySymbol),
    registerSettingShowVirtual :: !Bool,
    registerSettingBegin :: !(Maybe Day),
    registerSettingEnd :: !(Maybe Day)
  }
  deriving (Show)

instance HasParser RegisterSettings where
  settingsParser = parseRegisterSettings

{-# ANN parseRegisterSettings ("NOCOVER" :: String) #-}
parseRegisterSettings :: Parser RegisterSettings
parseRegisterSettings = subConfig_ "register" $ do
  registerSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          conf "convert",
          metavar "CURRENCY"
        ]
  registerSettingBlockSize <- settingsParser
  registerSettingShowVirtual <-
    setting
      [ help "Show virtual postings too",
        switch True,
        long "virtual",
        conf "virtual",
        value False
      ]
  ~(registerSettingBegin, registerSettingEnd) <- timeFilterParser
  registerSettingFilter <- settingsParser
  pure RegisterSettings {..}

data BalanceSettings = BalanceSettings
  { balanceSettingFilter :: !Filter,
    balanceSettingCurrency :: !(Maybe CurrencySymbol),
    balanceSettingShowEmpty :: !ShowEmpty,
    balanceSettingShowVirtual :: !Bool,
    balanceSettingEnd :: !(Maybe Day)
  }
  deriving (Show)

instance HasParser BalanceSettings where
  settingsParser = parseBalanceSettings

{-# ANN parseBalanceSettings ("NOCOVER" :: String) #-}
parseBalanceSettings :: Parser BalanceSettings
parseBalanceSettings = subConfig_ "balance" $ do
  balanceSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          conf "convert",
          metavar "CURRENCY"
        ]
  balanceSettingShowEmpty <-
    setting
      [ help "Show empty balances instead of hiding them",
        switch ShowEmpty,
        long "show-empty",
        conf "show-empty",
        value DoNotShowEmpty
      ]
  balanceSettingShowVirtual <-
    setting
      [ help "Show virtual postings too",
        switch True,
        long "virtual",
        conf "virtual",
        value False
      ]
  balanceSettingEnd <- endFilterParser
  balanceSettingFilter <- settingsParser
  pure BalanceSettings {..}

-- | Parser for time filtering with both begin and end dates (for register)
timeFilterParser :: Parser (Maybe Day, Maybe Day)
timeFilterParser =
  let getCurrentYear = do
        (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
        pure y
      yearTuple y = ((periodFirstDay :: Year -> Day) y, (periodLastDay :: Year -> Day) y)
      distributeMaybe =
        fmap
          ( \case
              Nothing -> (Nothing, Nothing)
              Just (b, e) -> (Just b, Just e)
          )
      yearParser =
        distributeMaybe $
          optional $
            choice
              [ yearTuple
                  <$> setting
                    [ help "Balance at the end of the given year",
                      name "year",
                      reader auto,
                      metavar "YEAR"
                    ],
                mapIO (\() -> yearTuple <$> getCurrentYear) $
                  setting
                    [ help "Balance at the end of the current year",
                      switch (),
                      long "this-year"
                    ],
                mapIO (\() -> yearTuple . (\y -> y - 1) <$> getCurrentYear) $
                  setting
                    [ help "Balance at the end of last year",
                      switch (),
                      long "last-year"
                    ]
              ]
      dayParser =
        (,)
          <$> optional
            ( setting
                [ help "Begin date (inclusive), in YYYY-MM-DD format",
                  option,
                  long "begin",
                  short 'b',
                  conf "begin",
                  reader auto,
                  metavar "DATE"
                ]
            )
          <*> optional
            ( setting
                [ help "End date (inclusive), in YYYY-MM-DD format",
                  option,
                  long "end",
                  short 'e',
                  conf "end",
                  reader auto,
                  metavar "DATE"
                ]
            )
      -- Combine year-based and day-based parsers
      -- If a year filter is set, it provides default begin/end dates
      -- Explicit --begin/--end override the year-based dates
      combineFilters (yearBegin, yearEnd) (dayBegin, dayEnd) =
        (dayBegin <|> yearBegin, dayEnd <|> yearEnd)
   in combineFilters <$> yearParser <*> dayParser

-- | Parser for end-date filtering only (for balance)
endFilterParser :: Parser (Maybe Day)
endFilterParser =
  let getCurrentYear = do
        (y, _, _) <- toGregorian . utctDay <$> getCurrentTime
        pure y
      yearEnd = (periodLastDay :: Year -> Day)
      yearParser =
        optional $
          choice
            [ yearEnd
                <$> setting
                  [ help "Balance at the end of the given year",
                    name "year",
                    reader auto,
                    metavar "YEAR"
                  ],
              mapIO (\() -> yearEnd <$> getCurrentYear) $
                setting
                  [ help "Balance at the end of the current year",
                    switch (),
                    long "this-year"
                  ],
              mapIO (\() -> yearEnd . (\y -> y - 1) <$> getCurrentYear) $
                setting
                  [ help "Balance at the end of last year",
                    switch (),
                    long "last-year"
                  ]
            ]
      dayParser =
        optional $
          setting
            [ help "End date (inclusive), in YYYY-MM-DD format",
              option,
              long "end",
              short 'e',
              reader auto,
              metavar "DATE"
            ]
      -- Combine year-based and day-based parsers
      -- Explicit --end overrides the year-based date
      combineFilters yearEnd' dayEnd = dayEnd <|> yearEnd'
   in combineFilters <$> yearParser <*> dayParser

data ShowEmpty
  = ShowEmpty
  | DoNotShowEmpty
  deriving (Show)

instance HasCodec ShowEmpty where
  codec = dimapCodec f g codec
    where
      f = \case
        True -> ShowEmpty
        False -> DoNotShowEmpty
      g = \case
        ShowEmpty -> True
        DoNotShowEmpty -> False

data FormatSettings = FormatSettings
  { formatSettingFileOrDir :: !(Maybe (Either (Path Abs File) (Path Abs Dir)))
  }
  deriving (Show)

instance HasParser FormatSettings where
  settingsParser = parseFormatSettings

{-# ANN parseFormatSettings ("NOCOVER" :: String) #-}
parseFormatSettings :: Parser FormatSettings
parseFormatSettings = do
  formatSettingFileOrDir <-
    optional $
      choice
        [ Left
            <$> filePathSetting
              [ help "File to format",
                option,
                short 'f',
                long "file"
              ],
          Right
            <$> directoryPathSetting
              [ help "Directory to format",
                option,
                short 'd',
                long "directory"
              ]
        ]
  pure FormatSettings {..}

data RatesGraphSettings = RatesGraphSettings
  { ratesGraphSettingOutputFile :: !(Maybe (Path Abs File))
  }
  deriving (Show)

instance HasParser RatesGraphSettings where
  settingsParser = parseRatesGraphSettings

{-# ANN parseRatesGraphSettings ("NOCOVER" :: String) #-}
parseRatesGraphSettings :: Parser RatesGraphSettings
parseRatesGraphSettings = do
  ratesGraphSettingOutputFile <-
    optional $
      filePathSetting
        [ help "Output file (default: stdout)",
          option,
          short 'o',
          long "output"
        ]
  pure RatesGraphSettings {..}
