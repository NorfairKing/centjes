{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Chart.Command.All (runCentjesChartAll) where

import qualified Centjes.AccountName as AccountName
import Centjes.Chart.OptParse
import Centjes.Compile
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import Centjes.CurrencySymbol
import Centjes.Filter
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Report.Balance
import Centjes.Report.Check
import Centjes.Report.Register
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Conduit
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Colour
import Data.Colour.SRGB
import Data.Function
import Data.List (foldl', nubBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Backend.Cairo as Chart
import Graphics.Rendering.Chart.Easy as Chart hiding (Vector)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Path
import Path.IO
import Paths_centjes_chart
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import Text.Show.Pretty
import Text.XML as XML

runCentjesChartAll :: Settings -> LoggingT IO ()
runCentjesChartAll Settings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    let symbol = CurrencySymbol "CHF"
    balancedLedger <-
      liftIO $
        checkValidation diagnostic $
          produceBalancedLedger
            False
            ledger
    let currency = fromJust $ do
          lqf <- M.lookup symbol $ ledgerCurrencies ledger
          pure $ Currency symbol lqf

    -- TODO Filter  by account type not name
    -- or better yet by actual filter
    let predicate = \an ->
          and
            [ "assets" `T.isInfixOf` AccountName.toText an,
              not ("self-transfer" `T.isInfixOf` AccountName.toText an)
            ]
    let dayMap = M.map (M.filterWithKey (\an _ -> predicate an)) (balancedLedgerToDayMap balancedLedger)

    let dailyPriceGraphs = pricesToDailyPriceGraphs (ledgerPrices ledger)
    convertedDayMap <- liftIO $ checkValidation diagnostic $ convertDayMap dailyPriceGraphs currency dayMap

    let stackMap = convertedToStackMap convertedDayMap

    let doubleMap = stackToDoubleMap currency stackMap

    let tups = accumulate doubleMap

    let fileOpts = def {_fo_format = SVG, _fo_size = (1920, 1080)}
    liftIO $
      void $
        renderableToFile fileOpts "assets.svg" $
          toRenderable $
            let stacks =
                  flip map (zip (cycle assetColors) tups) $ \(color, (an, trips)) ->
                    def
                      { _plot_fillbetween_values = flip map trips $ \(d, mv, v) -> (d, (fromMaybe 0 mv, v)),
                        _plot_fillbetween_style = solidFillStyle color,
                        _plot_fillbetween_title = AccountName.toString an
                      }
                layout =
                  layout_title .~ "Assets" $
                    layout_grid_last .~ True $
                      layout_plots .~ map toPlot stacks $
                        def
             in layout

type DayMap ann = Map Day (AccountBalances ann)

balancedLedgerToDayMap :: forall ann. (Ord ann) => BalancedLedger ann -> DayMap ann
balancedLedgerToDayMap = V.foldl' go M.empty . balancedLedgerTransactions
  where
    go ::
      DayMap ann ->
      (GenLocated ann (Transaction ann), AccountBalances ann) ->
      DayMap ann
    go dm (Located _ Transaction {..}, abs) =
      let Located _ timestamp = transactionTimestamp
          day = Timestamp.toDay timestamp
       in -- Here we only keep the latest balances in a day.
          M.insert day abs dm

type ConvertedDayMap = Map Day (Map AccountName Money.Account)

convertDayMap ::
  (Ord ann) =>
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Currency ann ->
  DayMap ann ->
  Validation (ConvertError ann) ConvertedDayMap
convertDayMap dailyPriceGraphs currency = M.traverseWithKey $ \day accountBalances ->
  let (_, priceGraph) = fromJust $ M.lookupLE day dailyPriceGraphs
   in for accountBalances $ \ma ->
        convertMultiAccountToAccount Nothing priceGraph currency ma

type StackMap = Map AccountName (Map Day Money.Account)

convertedToStackMap :: ConvertedDayMap -> StackMap
convertedToStackMap = flipMap

flipMap :: (Ord a, Ord b) => Map a (Map b c) -> Map b (Map a c)
flipMap = M.fromListWith M.union . concatMap (\(a, m) -> map (second (M.singleton a)) (M.toList m)) . M.toList

type DoubleMap = Map AccountName (Map Day Double)

stackToDoubleMap :: Currency ann -> StackMap -> DoubleMap
stackToDoubleMap currency = M.map $ M.map $ Account.toDouble qf
  where
    Located _ qf = currencyQuantisationFactor currency

accumulate :: DoubleMap -> [(AccountName, [(Day, Maybe Double, Double)])]
accumulate dm =
  let begin = []
   in foldl' go begin $ sortOn (volatility . snd) $ M.toList dm
  where
    daysInRange =
      let (db, de) = dayRange dm
       in [db .. de]
    go ::
      [(AccountName, [(Day, Maybe Double, Double)])] ->
      (AccountName, Map Day Double) ->
      [(AccountName, [(Day, Maybe Double, Double)])]
    go [] (a, ds) = [(a, ls)]
      where
        ls :: [(Day, Maybe Double, Double)]
        ls = do
          d <- daysInRange
          pure (d, Nothing, maybe 0 snd $ M.lookupLE d ds)
    go (a@(_, ads) : as) (acc, ds) = (acc, ls) : a : as
      where
        ls :: [(Day, Maybe Double, Double)]
        ls = do
          (d, _, av) <- ads
          pure (d, Just av, av + maybe 0 snd (M.lookupLE d ds))

volatility :: Map Day Double -> Double
volatility = go . M.toList
  where
    go [] = 0
    go [(_, _)] = 0
    go ((da, va) : (db, vb) : xs) =
      abs (vb - va)
        / ((fromIntegral :: Integer -> Double) (diffDays db da) * va)
        + go ((db, vb) : xs)

fromTupTups :: [(AccountName, [(Day, Double)])] -> DoubleMap
fromTupTups = foldl' (\m (a, vals) -> addValsToDoubleMap a vals m) emptyDoubleMap

emptyDoubleMap :: DoubleMap
emptyDoubleMap = M.empty

addValsToDoubleMap :: AccountName -> [(Day, Double)] -> DoubleMap -> DoubleMap
addValsToDoubleMap acc vals = M.insert acc $ M.fromList vals

dayRange :: DoubleMap -> (Day, Day)
dayRange = go . concatMap snd . M.toList . M.map (S.toList . M.keysSet)
  where
    go :: [Day] -> (Day, Day)
    go ts = (minimum ts, maximum ts)

undupDays :: [(Day, a)] -> [(Day, a)]
undupDays = reverse . nubBy ((==) `on` fst) . reverse

assetColors :: [AlphaColour Double]
assetColors =
  map
    opaque
    [ sRGB24 166 206 227,
      sRGB24 31 120 180,
      sRGB24 178 223 138,
      sRGB24 51 160 44,
      sRGB24 251 154 153,
      sRGB24 227 26 28,
      sRGB24 253 191 111,
      sRGB24 255 127 0,
      sRGB24 202 178 214,
      sRGB24 106 61 154,
      sRGB24 255 255 153,
      sRGB24 177 89 40,
      sRGB24 66 206 227,
      sRGB24 1 120 180,
      sRGB24 178 223 138,
      sRGB24 1 160 44,
      sRGB24 51 154 153,
      sRGB24 27 26 28,
      sRGB24 53 191 111,
      sRGB24 55 127 0,
      sRGB24 02 178 214,
      sRGB24 06 61 154,
      sRGB24 55 255 153,
      sRGB24 77 89 40
    ]
