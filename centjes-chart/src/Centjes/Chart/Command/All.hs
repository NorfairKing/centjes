{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Chart.Command.All (runCentjesChartAll) where

import Centjes.Chart.OptParse
import Centjes.Compile
import Centjes.Load
import Centjes.Report.Check
import Centjes.Validation
import Conduit
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
import Graphics.Rendering.Chart.Backend.Cairo as Chart
import Graphics.Rendering.Chart.Easy as Chart
import Path
import Path.IO
import Paths_centjes_switzerland
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import Text.Show.Pretty
import Text.XML as XML

runCentjesChartAll :: Settings -> IO ()
runCentjesChartAll Settings {..} = do
  let stackMap =
        fromTupTups
          [ ("Assets", [(fromGregorian 2020 1 1, 100), (fromGregorian 2020 2 1, 110), (fromGregorian 2020 3 1, 120)]),
            ("Equity", [(fromGregorian 2020 1 1, 50), (fromGregorian 2020 2 1, 50), (fromGregorian 2020 3 1, 50)])
          ]
  let fileOpts = def {_fo_format = SVG, _fo_size = (960, 540)} -- (1920, 1080)}
  void $
    renderableToFile fileOpts "example.svg" $
      toRenderable $
        let stacks =
              flip map (zip (cycle assetColors) (accumulate stackMap)) $ \(color, (a, trips)) ->
                def
                  { _plot_fillbetween_values = flip map trips $ \(d, mv, v) -> (d, (fromMaybe 0 mv, v)),
                    _plot_fillbetween_style = solidFillStyle color,
                    _plot_fillbetween_title = T.unpack a
                  }
            layout =
              layout_title .~ "Assets" $
                layout_grid_last .~ True $
                  layout_plots .~ map toPlot stacks $
                    def
         in layout

type StackMap = Map Text (Map Day Double)

accumulate :: StackMap -> [(Text, [(Day, Maybe Double, Double)])]
accumulate sm =
  let begin = []
   in foldl' go begin $ sortOn (volatility . snd) $ M.toList sm
  where
    daysInRange =
      let (db, de) = dayRange sm
       in [db .. de]
    go ::
      [(Text, [(Day, Maybe Double, Double)])] ->
      (Text, Map Day Double) ->
      [(Text, [(Day, Maybe Double, Double)])]
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

fromTupTups :: [(Text, [(Day, Double)])] -> StackMap
fromTupTups = foldl' (\m (a, vals) -> addValsToStackMap a vals m) emptyStackMap

emptyStackMap :: StackMap
emptyStackMap = M.empty

addValsToStackMap :: Text -> [(Day, Double)] -> StackMap -> StackMap
addValsToStackMap acc vals = M.insert acc $ M.fromList vals

dayRange :: StackMap -> (Day, Day)
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
