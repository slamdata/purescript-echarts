module Events where

import Prelude
import Control.Monad.Eff.Console (print, CONSOLE())
import Math hiding (log)
import Data.Array hiding (init)
import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Utils

import ECharts.Chart
import ECharts.Events
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import qualified  ECharts.DataZoom as Zoom

simpleData = Value <<< Simple

lineData :: Eff _ (Array Number)
lineData = do
  lst <- randomLst 30.0
  return $ (\x -> round $ x * 30.0 + 30.0 ) <$> lst

barData :: Eff _ (Array Number)
barData = do
  lst <- randomLst 30.0
  return $ (\x -> round $ x * 10.0) <$> lst

options_ :: Array Number -> Array Number -> Option
options_ line bar = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  legend = Just $ Legend legendDefault {
    "data" = Just $ legendItemDefault <$> ["fst","snd"]
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just true,
    x = Just XRight,
    feature = Just $ Feature $ featureDefault {
      mark = Just $ MarkFeature $ markFeatureDefault {show = Just true},
      dataView = Just $ DataViewFeature $ dataViewFeatureDefault {
        show = Just true,
        readOnly = Just false
        },
      magicType = Just $ MagicTypeFeature $ magicTypeFeatureDefault {
        show = Just true,
        "type" = Just [MagicLine, MagicBar, MagicStack, MagicTiled]
        },
      restore = Just $ RestoreFeature $ restoreFeatureDefault {
        show = Just true
        },
      saveAsImage = Just $ SaveAsImageFeature $ saveAsImageFeatureDefault {
        show = Just true
        }
      }
    },
  calculable = Just true,
  dataZoom = Just $ Zoom.DataZoom $ Zoom.dataZoomDefault {
    show = Just true,
    realtime = Just true,
    start = Just 40.0,
    end = Just 60.0
    },
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    boundaryGap = Just $ CatBoundaryGap true,
    "data" = Just $ (\i -> CommonAxisData $ "2013-03-" <> show i) <$> (1..30)
    },
  yAxis = Just $ OneAxis $ Axis $ axisDefault {"type" = Just ValueAxis},
  series = Just $ Just <$> [
    LineSeries {
       common: universalSeriesDefault{name = Just "fst"},
       lineSeries: lineSeriesDefault{"data" = Just $ simpleData <$> line}
       },
    BarSeries {
      common: universalSeriesDefault{name = Just "snd"},
      barSeries: barSeriesDefault{"data" = Just $ simpleData <$> bar}
      }
    ]
  }


options :: Eff _ _
options = do
  line <- lineData
  bar <- barData
  return $ options_ line bar

foreign import log :: forall a e. a -> Eff e Unit


subscribe chart = do
  let sub = \et hndl -> listen et hndl chart
  sub ClickEvent log
  sub DoubleClickEvent log
  sub DataZoomEvent log
  sub LegendSelectedEvent log
  sub MagicTypeChangedEvent log
  sub DataViewChangedEvent log


events id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in events"
    Just el -> do
      opts <- options
      chart <- init Nothing el
               >>= setOption opts true

      subscribe chart
      return unit


