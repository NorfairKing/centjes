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
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.WeekDate
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

data OutputFormat
  = OutputFormatTerminal
  | OutputFormatCSV
  deriving (Show)

data Dispatch
  = DispatchCheck !CheckSettings
  | DispatchRegister !RegisterSettings
  | DispatchBalance !BalanceSettings
  | DispatchNetWorth !NetWorthSettings
  | DispatchIncomeStatement !IncomeStatementSettings
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
      command "net-worth" "net worth report" $ DispatchNetWorth <$> settingsParser,
      command "income-statement" "income statement report" $ DispatchIncomeStatement <$> settingsParser,
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

data NetWorthSettings = NetWorthSettings
  { netWorthSettingCurrency :: !(Maybe CurrencySymbol),
    netWorthSettingBegin :: !(Maybe Day),
    netWorthSettingEnd :: !(Maybe Day),
    netWorthSettingOutputFormat :: !OutputFormat
  }
  deriving (Show)

instance HasParser NetWorthSettings where
  settingsParser = parseNetWorthSettings

{-# ANN parseNetWorthSettings ("NOCOVER" :: String) #-}
parseNetWorthSettings :: Parser NetWorthSettings
parseNetWorthSettings = subConfig_ "net-worth" $ do
  netWorthSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          conf "convert",
          metavar "CURRENCY"
        ]
  netWorthSettingOutputFormat <-
    setting
      [ help "Output as CSV",
        switch OutputFormatCSV,
        long "csv",
        value OutputFormatTerminal
      ]
  ~(netWorthSettingBegin, netWorthSettingEnd) <- timeFilterParser
  pure NetWorthSettings {..}

data IncomeStatementSettings = IncomeStatementSettings
  { incomeStatementSettingFilter :: !Filter,
    incomeStatementSettingCurrency :: !(Maybe CurrencySymbol),
    incomeStatementSettingShowEmpty :: !ShowEmpty,
    incomeStatementSettingShowVirtual :: !Bool,
    incomeStatementSettingBegin :: !(Maybe Day),
    incomeStatementSettingEnd :: !(Maybe Day)
  }
  deriving (Show)

instance HasParser IncomeStatementSettings where
  settingsParser = parseIncomeStatementSettings

{-# ANN parseIncomeStatementSettings ("NOCOVER" :: String) #-}
parseIncomeStatementSettings :: Parser IncomeStatementSettings
parseIncomeStatementSettings = subConfig_ "income-statement" $ do
  incomeStatementSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          conf "convert",
          metavar "CURRENCY"
        ]
  incomeStatementSettingShowEmpty <-
    setting
      [ help "Show empty balances instead of hiding them",
        switch ShowEmpty,
        long "show-empty",
        conf "show-empty",
        value DoNotShowEmpty
      ]
  incomeStatementSettingShowVirtual <-
    setting
      [ help "Show virtual postings too",
        switch True,
        long "virtual",
        conf "virtual",
        value False
      ]
  ~(incomeStatementSettingBegin, incomeStatementSettingEnd) <- timeFilterParser
  incomeStatementSettingFilter <- settingsParser
  pure IncomeStatementSettings {..}

-- | Parser for time filtering with both begin and end dates (for register)
timeFilterParser :: Parser (Maybe Day, Maybe Day)
timeFilterParser =
  let getCurrentDay = utctDay <$> getCurrentTime
      getCurrentYear = do
        (y, _, _) <- toGregorian <$> getCurrentDay
        pure y
      yearTuple y = ((periodFirstDay :: Year -> Day) y, (periodLastDay :: Year -> Day) y)
      quarterTuple q = ((periodFirstDay :: Quarter -> Day) q, (periodLastDay :: Quarter -> Day) q)
      monthTuple m = ((periodFirstDay :: Month -> Day) m, (periodLastDay :: Month -> Day) m)
      weekTuple d =
        let (y, w, _) = toWeekDate d
         in (fromWeekDate y w 1, fromWeekDate y w 7)
      distributeMaybe =
        fmap
          ( \case
              Nothing -> (Nothing, Nothing)
              Just (b, e) -> (Just b, Just e)
          )
      periodParser =
        distributeMaybe $
          optional $
            choice
              [ yearTuple
                  <$> setting
                    [ help "Filter to the given year",
                      name "year",
                      reader auto,
                      metavar "YEAR"
                    ],
                mapIO (\() -> yearTuple <$> getCurrentYear) $
                  setting
                    [ help "Filter to the current year",
                      switch (),
                      long "this-year"
                    ],
                mapIO (\() -> yearTuple . (\y -> y - 1) <$> getCurrentYear) $
                  setting
                    [ help "Filter to last year (beginning to end of last year)",
                      switch (),
                      long "last-year"
                    ],
                mapIO (\() -> (\today -> (addDays (-364) today, today)) <$> getCurrentDay) $
                  setting
                    [ help "Filter to the past year (past 365 days)",
                      switch (),
                      long "past-year"
                    ],
                mapIO (\() -> quarterTuple . (dayPeriod :: Day -> Quarter) <$> getCurrentDay) $
                  setting
                    [ help "Filter to the current quarter",
                      switch (),
                      long "this-quarter"
                    ],
                mapIO (\() -> quarterTuple . pred . (dayPeriod :: Day -> Quarter) <$> getCurrentDay) $
                  setting
                    [ help "Filter to last quarter (beginning to end of last quarter)",
                      switch (),
                      long "last-quarter"
                    ],
                mapIO (\() -> (\today -> (addDays (-89) today, today)) <$> getCurrentDay) $
                  setting
                    [ help "Filter to the past quarter (past 90 days)",
                      switch (),
                      long "past-quarter"
                    ],
                mapIO (\() -> monthTuple . dayPeriod <$> getCurrentDay) $
                  setting
                    [ help "Filter to the current month",
                      switch (),
                      long "this-month"
                    ],
                mapIO (\() -> monthTuple . pred . dayPeriod <$> getCurrentDay) $
                  setting
                    [ help "Filter to last month (beginning to end of last month)",
                      switch (),
                      long "last-month"
                    ],
                mapIO (\() -> (\today -> (addDays (-29) today, today)) <$> getCurrentDay) $
                  setting
                    [ help "Filter to the past month (past 30 days)",
                      switch (),
                      long "past-month"
                    ],
                mapIO (\() -> weekTuple <$> getCurrentDay) $
                  setting
                    [ help "Filter to the current week",
                      switch (),
                      long "this-week"
                    ],
                mapIO (\() -> weekTuple . addDays (-7) <$> getCurrentDay) $
                  setting
                    [ help "Filter to last week (beginning to end of last week)",
                      switch (),
                      long "last-week"
                    ],
                mapIO (\() -> (\today -> (addDays (-6) today, today)) <$> getCurrentDay) $
                  setting
                    [ help "Filter to the past week (past 7 days)",
                      switch (),
                      long "past-week"
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
      -- Combine period-based and day-based parsers
      -- If a period filter is set, it provides default begin/end dates
      -- Explicit --begin/--end override the period-based dates
      combineFilters (periodBegin, periodEnd) (dayBegin, dayEnd) =
        (dayBegin <|> periodBegin, dayEnd <|> periodEnd)
   in combineFilters <$> periodParser <*> dayParser

-- | Parser for end-date filtering only (for balance)
endFilterParser :: Parser (Maybe Day)
endFilterParser =
  let getCurrentDay = utctDay <$> getCurrentTime
      getCurrentYear = do
        (y, _, _) <- toGregorian <$> getCurrentDay
        pure y
      periodParser =
        optional $
          choice
            [ (periodLastDay :: Year -> Day)
                <$> setting
                  [ help "Balance at the end of the given year",
                    name "year",
                    reader auto,
                    metavar "YEAR"
                  ],
              mapIO (\() -> (periodLastDay :: Year -> Day) <$> getCurrentYear) $
                setting
                  [ help "Balance at the end of the current year",
                    switch (),
                    long "this-year"
                  ],
              mapIO (\() -> (periodLastDay :: Year -> Day) . (\y -> y - 1) <$> getCurrentYear) $
                setting
                  [ help "Balance at the end of last year",
                    switch (),
                    long "last-year"
                  ],
              mapIO (\() -> getCurrentDay) $
                setting
                  [ help "Balance at the end of the past year (today)",
                    switch (),
                    long "past-year"
                  ],
              mapIO (\() -> (periodLastDay :: Quarter -> Day) . (dayPeriod :: Day -> Quarter) <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of the current quarter",
                    switch (),
                    long "this-quarter"
                  ],
              mapIO (\() -> (periodLastDay :: Quarter -> Day) . pred . (dayPeriod :: Day -> Quarter) <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of last quarter",
                    switch (),
                    long "last-quarter"
                  ],
              mapIO (\() -> getCurrentDay) $
                setting
                  [ help "Balance at the end of the past quarter (today)",
                    switch (),
                    long "past-quarter"
                  ],
              mapIO (\() -> (periodLastDay :: Month -> Day) . dayPeriod <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of the current month",
                    switch (),
                    long "this-month"
                  ],
              mapIO (\() -> (periodLastDay :: Month -> Day) . pred . dayPeriod <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of last month",
                    switch (),
                    long "last-month"
                  ],
              mapIO (\() -> getCurrentDay) $
                setting
                  [ help "Balance at the end of the past month (today)",
                    switch (),
                    long "past-month"
                  ],
              mapIO (\() -> (\d -> let (y, w, _) = toWeekDate d in fromWeekDate y w 7) <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of the current week",
                    switch (),
                    long "this-week"
                  ],
              mapIO (\() -> (\d -> let (y, w, _) = toWeekDate (addDays (-7) d) in fromWeekDate y w 7) <$> getCurrentDay) $
                setting
                  [ help "Balance at the end of last week",
                    switch (),
                    long "last-week"
                  ],
              mapIO (\() -> getCurrentDay) $
                setting
                  [ help "Balance at the end of the past week (today)",
                    switch (),
                    long "past-week"
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
      -- Combine period-based and day-based parsers
      -- Explicit --end overrides the period-based date
      combineFilters periodEnd dayEnd = dayEnd <|> periodEnd
   in combineFilters <$> periodParser <*> dayParser

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
