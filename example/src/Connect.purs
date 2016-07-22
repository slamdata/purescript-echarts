module Connect where

import Prelude
import Control.Monad.Eff.Console
import Data.Tuple
import Utils
import Data.Maybe


import ECharts.Chart
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
import ECharts.Connect
import ECharts.Title
import ECharts.Grid


simpleData = Value <<< Simple
options1 = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {
     trigger = Just TriggerItem,
     formatter = Just $ Template "{a} <br/> {b}: {c} ({d}%)"
     },
  title = Just $ Title titleDefault {
    text = Just "a site user to access source",
    subtext = Just "fictitious",
    x = Just XRight
    },
  legend = Just $ Legend legendDefault {
    orient = Just Vertical,
    x = Just XLeft,
    "data" = Just $ legendItemDefault <$>
              ["direct access", "email marketing",
               "affiliate advertising", "video ads", "search engine"]
    },
  calculable = Just true,
  series = Just $ Just <$> [
    PieSeries {
       common: universalSeriesDefault {
          name = Just "access to the source."
          },
       pieSeries: pieSeriesDefault {
         radius = Just $ R (Percent 55.0),
         center = Just $ Tuple (Percent 50.0) (Pixel 225.0),
         "data" = Just $ [
           Dat $ (dataDefault $ Simple 335.0) {name = Just "direct access"},
           Dat $ (dataDefault $ Simple 310.0) {name = Just "email marketing"},
           Dat $ (dataDefault $ Simple 234.0) {name = Just "affiliate advertising"},
           Dat $ (dataDefault $ Simple 135.0) {name = Just "video ads"},
           Dat $ (dataDefault $ Simple 158.0) {name = Just "search engine"}
           ]

         }
       }
    ]
  }

options2 = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {
     trigger = Just TriggerAxis,
     axisPointer = Just $ TooltipAxisPointer tooltipAxisPointerDefault {
       "type" = Just ShadowPointer
       }
     },
  legend = Just $ Legend $ legendDefault {
    "data" = Just $ legendItemDefault <$>
           ["direct access", "email marketing",
            "affiliate advertising", "video ads", "search engine"]
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    feature = Just $ Feature $ featureDefault {
       mark = Just $ MarkFeature $ markFeatureDefault {show = Just true},
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
       },
    show = Just true,
    y = Just YBottom
  },
  calculable = Just true,
  xAxis = Just $ OneAxis $ Axis axisDefault {
     "type" = Just CategoryAxis,
     "data" = Just $ CommonAxisData <$>
              ["Monday", "Tuesday", "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday"]
     },
  yAxis = Just $ OneAxis $ Axis axisDefault {
    "type" = Just ValueAxis,
    splitArea = Just $ AxisSplitArea axisSplitAreaDefault {
      show = Just true
      }
    },
  grid = Just $ Grid gridDefault {x2 = Just $ Pixel 40.0},
  series = Just $ Just <$> [
    BarSeries {
       common: universalSeriesDefault {
          name = Just "direct access"
          },
       barSeries: barSeriesDefault {
         stack = Just "total",
         "data" = Just $ simpleData <$> [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0]
         }
       },
    BarSeries {
      common: universalSeriesDefault {
         name = Just "email marketing"
         },
      barSeries: barSeriesDefault {
        stack = Just "email marketing",
        "data" = Just $ simpleData <$> [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         name = Just "affiliate advertising"
         },
      barSeries: barSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         name = Just "video Ads"
         },
      barSeries: barSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         name = Just "search engine"
         },
      barSeries: barSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0]
        }
      }
    ]
  }

conn first second = do
  connect first second
  connect second first


connectM firstId secondId = do
  mbElFst <- getElementById firstId
  mbElSnd <- getElementById secondId

  case Tuple mbElFst mbElSnd  of
    Tuple Nothing _ -> log "incorrect first id in connect"
    Tuple _ Nothing -> log "incorrect second id in connect"
    Tuple (Just first) (Just second) -> do
      fst <- init Nothing first >>= setOption options1 true
      snd <- init Nothing second >>= setOption options2 true
      conn fst snd
      return unit

