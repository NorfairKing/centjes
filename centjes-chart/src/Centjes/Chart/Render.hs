{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Chart.Render
  ( RenderSettings (..),
    renderChartReportSvg,
    stackBands,
    lineSeries,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Chart.Report
import Centjes.Ledger
import Centjes.Location
import Data.Colour
import Data.Colour.SRGB
import Data.Time (Day)
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Backend.Diagrams (FileFormat (..), FileOptions (..), renderableToFile)
import Graphics.Rendering.Chart.Easy hiding (Path, Vector)
import qualified Money.Account as Account
import Path
import Path.IO (ensureDir)

-- | How to render a chart report, beyond the data itself.
data RenderSettings = RenderSettings
  { renderSettingTitle :: !String,
    renderSettingWidth :: !Double,
    renderSettingHeight :: !Double
  }

-- | Render a chart report to an SVG file.
--
-- A stacked area chart is used when every value is non-negative; otherwise the
-- series are drawn as overlaid lines, since stacking signed values produces a
-- misleading picture.
--
-- This is the only place where the exact monetary values become 'Double': the
-- charting library cannot use precise money, so we convert here, at the very
-- edge, and nowhere earlier.
renderChartReportSvg :: RenderSettings -> ChartReport ann -> Path Abs File -> IO ()
renderChartReportSvg RenderSettings {..} report outputFile = do
  let Located _ quantisationFactor = currencyQuantisationFactor (chartReportCurrency report)
      toDouble :: Account.Account -> Double
      toDouble = Account.toDouble quantisationFactor
      xAxisDays = V.toList (chartReportDays report)
      seriesList = V.toList (chartReportSeries report)
      fileOptions =
        def
          { _fo_format = SVG,
            _fo_size = (renderSettingWidth, renderSettingHeight)
          }
      plots
        | chartReportIsNonNegative report =
            flip map (zip (cycle assetColors) (stackBands toDouble xAxisDays seriesList)) $
              \(color, (title, bandPoints)) ->
                toPlot $
                  def
                    { _plot_fillbetween_values = bandPoints,
                      _plot_fillbetween_style = solidFillStyle color,
                      _plot_fillbetween_title = title
                    }
        | otherwise =
            flip map (zip (cycle assetColors) (lineSeries toDouble xAxisDays seriesList)) $
              \(color, (title, linePoints)) ->
                toPlot $
                  def
                    { _plot_lines_values = [linePoints],
                      _plot_lines_style = solidLine 2 color,
                      _plot_lines_title = title
                    }
      layout =
        layout_title .~ renderSettingTitle $
          layout_grid_last .~ True $
            layout_plots .~ plots $
              def
  ensureDir (parent outputFile)
  _ <- renderableToFile fileOptions (toFilePath outputFile) (toRenderable layout)
  pure ()

-- | Stack the series on top of one another: each series' band runs from the
-- running total below it to that total plus the series' own value.
stackBands ::
  (Account.Account -> Double) ->
  [Day] ->
  [ChartSeries ann] ->
  [(String, [(Day, (Double, Double))])]
stackBands toDouble xAxisDays = go (map (const 0) xAxisDays)
  where
    go _ [] = []
    go lowers (s : rest) =
      let values = map toDouble (V.toList (chartSeriesValues s))
          uppers = zipWith (+) lowers values
          bandPoints = zipWith3 (\day lower upper -> (day, (lower, upper))) xAxisDays lowers uppers
       in (AccountName.toString (chartSeriesAccount s), bandPoints) : go uppers rest

-- | Each series as its own line of @(day, value)@ points.
lineSeries ::
  (Account.Account -> Double) ->
  [Day] ->
  [ChartSeries ann] ->
  [(String, [(Day, Double)])]
lineSeries toDouble xAxisDays =
  map $ \s ->
    ( AccountName.toString (chartSeriesAccount s),
      zip xAxisDays (map toDouble (V.toList (chartSeriesValues s)))
    )

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
      sRGB24 177 89 40
    ]
