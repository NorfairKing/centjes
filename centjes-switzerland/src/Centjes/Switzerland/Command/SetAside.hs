{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Switzerland.Command.SetAside
  ( runCentjesSwitzerlandSetAside,
    splitPrices,
    renderSetAsideReport,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.SetAside
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Semigroup (Max (..))
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import Data.Word
import qualified Money.Account as Money (Account)
import qualified Money.Amount as Amount
import Money.QuantisationFactor (QuantisationFactor (..))
import Path
import Text.Colour
import Text.Colour.Layout

runCentjesSwitzerlandSetAside :: Settings -> SetAsideSettings -> IO ()
runCentjesSwitzerlandSetAside Settings {..} SetAsideSettings {..} =
  runStderrLoggingT $
    loadMWatchedModules settingWatch (settingBaseDir </> settingLedgerFile) $ \(declarations, fileMap) -> do
      let diag = diagFromFileMap fileMap
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations
      today <- liftIO $ utctDay <$> getCurrentTime
      report <-
        liftIO $
          checkValidation diag $
            produceSetAsideReport setAsideSettingInput today (splitPrices settingPricesFile ledger) ledger
      liftIO $
        putChunksLocaleWith setAsideSettingTerminalCapabilities $
          renderSetAsideReport report

-- | The prices file holds the ESTV's prices, as downloaded by the
-- download-rates command, and every other file holds market prices.
splitPrices :: Path Rel File -> Ledger SourceSpan -> SetAsidePrices SourceSpan
splitPrices pricesFile ledger =
  let (setAsidePricesOfficial, setAsidePricesMarket) =
        V.partition (\(Located loc _) -> sourceSpanFile loc == pricesFile) (ledgerPrices ledger)
   in SetAsidePrices {..}

-- | One section per tax, whose rows add up to its surplus, and what to do
-- about it.
--
-- Each section starts with what its percentages are of: the revenue, or the
-- taxable wealth and the simple tax, which are not part of that sum.
renderSetAsideReport :: SetAsideReport -> [Chunk]
renderSetAsideReport SetAsideReport {..} =
  let WealthTaxBasis {..} = setAsideReportWealthTaxBasis
      year :: T.Text
      year = T.pack (show setAsideReportYear)
      taxLines :: [SetAsideLine]
      taxLines = [setAsideReportWealthTax, setAsideReportSocialSecurity, setAsideReportCantonal, setAsideReportFederal]
      percentageWidth :: Int
      percentageWidth =
        maximum (0 : [T.length percentage | SetAsideLine {..} <- taxLines, Just percentage <- map rowPercentage setAsideLineRows])
      amountWidth :: Max Word8
      amountWidth =
        foldMap accountWidth $
          setAsideReportRevenue
            : wealthTaxBasisTaxableWealth
            : wealthTaxBasisSimpleTax
            : concatMap (\SetAsideLine {..} -> setAsideLineSurplus : map setAsideRowAmount setAsideLineRows) taxLines
      row :: Chunk -> Maybe T.Text -> Money.Account -> [Chunk]
      row label mPercentage account =
        [ label,
          maybe "" (chunk . T.justifyRight percentageWidth ' ') mPercentage,
          maybe "" (const "%") mPercentage,
          accountChunkWithWidth (Just amountWidth) chfQuantisationFactor account,
          "CHF"
        ]
      emptyRow :: [Chunk]
      emptyRow = replicate 5 ""
      section :: T.Text -> [[Chunk]] -> SetAsideLine -> [[Chunk]]
      section title basisRows SetAsideLine {..} =
        concat
          [ [fore yellow (chunk title) : replicate 4 ""],
            basisRows,
            map (\r -> row (indented (rowLabel r)) (rowPercentage r) (setAsideRowAmount r)) setAsideLineRows,
            [ row ((fore white . bold) "  Surplus") Nothing setAsideLineSurplus,
              emptyRow
            ]
          ]
      revenueRow :: [Chunk]
      revenueRow = row (indented "Revenue") Nothing setAsideReportRevenue
      wealthTaxTitle :: T.Text
      wealthTaxTitle = T.concat ["Wealth tax ", year, ", on wealth of ", T.pack (show wealthTaxBasisDay)]
      rows :: [[Chunk]]
      rows =
        concat
          [ section
              wealthTaxTitle
              [ row (indented "Taxable wealth") Nothing wealthTaxBasisTaxableWealth,
                row (indented "Simple wealth tax") Nothing wealthTaxBasisSimpleTax
              ]
              setAsideReportWealthTax,
            section (T.unwords ["SVA", year]) [revenueRow] setAsideReportSocialSecurity,
            section (T.unwords ["Cantonal", year]) [revenueRow] setAsideReportCantonal,
            section (T.unwords ["Federal", year]) [revenueRow] setAsideReportFederal
          ]
   in renderTable (table (map (map pure) rows))
        ++ ["\n", fore yellow "To do", "\n"]
        ++ renderActions setAsideReportActions setAsideReportSurpluses

rowLabel :: SetAsideRow -> T.Text
rowLabel = \case
  SetAsideRowRevenueShare an _ _ -> AccountName.toText an
  SetAsideRowCantonalWealthTax _ _ -> "Cantonal Steuerfuss"
  SetAsideRowMunicipalWealthTax _ _ -> "Municipal Steuerfuss"
  SetAsideRowReserve an _ -> AccountName.toText an

rowPercentage :: SetAsideRow -> Maybe T.Text
rowPercentage = \case
  SetAsideRowRevenueShare _ rate _ -> Just (T.pack (showPercentage rate))
  SetAsideRowCantonalWealthTax steuerfuss _ -> Just (T.pack (show steuerfuss))
  SetAsideRowMunicipalWealthTax steuerfuss _ -> Just (T.pack (show steuerfuss))
  SetAsideRowReserve _ _ -> Nothing

indented :: T.Text -> Chunk
indented t = fore white $ chunk $ T.concat ["  ", t]

-- | One line per tax that needs more money set aside, and one per tax that
-- has more than it needs.
renderActions :: [SetAsideAction] -> [SetAsideSurplus] -> [Chunk]
renderActions actions surpluses =
  concat
    [ ["  Nothing to set aside, every tax has enough.\n" | null actions],
      concatMap
        ( \SetAsideAction {..} ->
            [ "  ",
              chunk setAsideActionTax,
              case setAsideActionKind of
                SetAsideActionSave -> ": save "
                SetAsideActionPrepay -> ": pre-pay ",
              fore red $ chunk $ T.pack $ Amount.format chfQuantisationFactor setAsideActionAmount,
              chunk $ T.concat [" CHF (", T.pack (Amount.format chfQuantisationFactor setAsideActionShortfall), " CHF short)"]
            ]
              ++ maybe
                []
                ( \an ->
                    [ case setAsideActionKind of
                        SetAsideActionSave -> ", into "
                        SetAsideActionPrepay -> ", booked on ",
                      fore white (chunk (AccountName.toText an))
                    ]
                )
                setAsideActionAccount
              ++ [".\n"]
        )
        actions,
      concatMap
        ( \SetAsideSurplus {..} ->
            [ "  ",
              chunk setAsideSurplusTax,
              ": ",
              fore green $ chunk $ T.pack $ Amount.format chfQuantisationFactor setAsideSurplusAmount,
              " CHF more set aside than needed.\n"
            ]
        )
        surpluses
    ]

chfQuantisationFactor :: QuantisationFactor
chfQuantisationFactor = QuantisationFactor 100
