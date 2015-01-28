module Events where

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
import ECharts.Type
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import qualified  ECharts.DataZoom as Zoom

simpleData = Value <<< Simple

lineData :: Eff _ [Number]
lineData = do 
  lst <- randomLst 30
  return $ (\x -> round $ x * 30 + 30 ) <$> lst

barData :: Eff _ [Number]
barData = do
  lst <- randomLst 30
  return $ (\x -> round $ x * 10) <$> lst

options_ :: [Number] -> [Number] -> Option
options_ line bar = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  legend = Just $ Legend legendDefault {
    "data" = Just $ legendItemDefault <$> ["fst","snd"]
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    "show" = Just true,
    "x" = Just XRight,
    "feature" = Just $ Feature $ featureDefault {
      "mark" = Just $ MarkFeature $ markFeatureDefault {show = Just true},
      "dataView" = Just $ DataViewFeature $ dataViewFeatureDefault {
        "show" = Just true,
        "readOnly" = Just false
        },
      "magicType" = Just $ MagicTypeFeature $ magicTypeFeatureDefault {
        "show" = Just true,
        "type" = Just [MagicLine, MagicBar, MagicStack, MagicTiled]
        },
      "restore" = Just $ RestoreFeature $ restoreFeatureDefault {
        "show" = Just true
        },
      "saveAsImage" = Just $ SaveAsImageFeature $ saveAsImageFeatureDefault {
        "show" = Just true
        }
      }
    },
  "calculable" = Just true,
  "dataZoom" = Just $ Zoom.DataZoom $ Zoom.dataZoomDefault {
    "show" = Just true,
    "realtime" = Just true,
    "start" = Just 40,
    "end" = Just 60
    },
  "xAxis" = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    "boundaryGap" = Just $ CatBoundaryGap true,
    "data" = Just $ (\i -> CommonAxisData $ "2013-03-" <> show i) <$> (1..30) 
    },
  "yAxis" = Just $ OneAxis $ Axis $ axisDefault {"type" = Just ValueAxis},
  "series" = Just $ Just <$> [
    LineSeries {
       "common": universalSeriesDefault{"name" = Just "fst"},
       "special": lineSeriesDefault{"data" = Just $ simpleData <$> line}
       },
    BarSeries {
      "common": universalSeriesDefault{"name" = Just "snd"},
      "special": barSeriesDefault{"data" = Just $ simpleData <$> bar}
      }
    ]
  }


options :: Eff _ _ 
options = do
  line <- lineData
  bar <- barData
  return $ options_ line bar


subscribe chart = do 
  let sub = \et hndl -> listen et hndl chart
  sub Click log
  sub DoubleClick log
  sub DataZoom log
  sub LegendSelected log
  sub MagicTypeChanged log
  sub DataViewChanged log


events id = do
  opts <- options
  chart <- getElementById id
           >>= init Nothing
           >>= setOption opts true

  subscribe chart
  return unit

