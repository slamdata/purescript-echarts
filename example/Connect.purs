module Connect where

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
import ECharts.Type
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
    x = Just XLeft
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
       special: pieSeriesDefault {
         radius = Just $ R (Percent 55),
         center = Just $ Tuple (Percent 50) (Pixel 225),
         "data" = Just $ [
           Dat $ (dataDefault $ Simple 335) {name = Just "direct access"},
           Dat $ (dataDefault $ Simple 310) {name = Just "email marketing"},
           Dat $ (dataDefault $ Simple 234) {name = Just "affiliate advertising"},
           Dat $ (dataDefault $ Simple 135) {name = Just "video ads"},
           Dat $ (dataDefault $ Simple 158) {name = Just "search engine"}
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
  "toolbox" = Just $ Toolbox $ toolboxDefault {
    "feature" = Just $ Feature $ featureDefault {
       "mark" = Just $ MarkFeature $ markFeatureDefault {show = Just true},
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
       },
    "show" = Just true
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
    "splitArea" = Just $ AxisSplitArea axisSplitAreaDefault {
      "show" = Just true
      }
    },
  grid = Just $ Grid gridDefault {"x2" = Just $ Pixel 40},
  series = Just $ Just <$> [
    BarSeries {
       common: universalSeriesDefault {
          "name" = Just "direct access"
          },
       special: barSeriesDefault {
         "stack" = Just "total",
         "data" = Just $ simpleData <$> [320, 332, 301, 334, 390, 330, 320]
         }
       },
    BarSeries {
      common: universalSeriesDefault {
         "name" = Just "email marketing"
         },
      special: barSeriesDefault {
        "stack" = Just "email marketing",
        "data" = Just $ simpleData <$> [120, 132, 101, 134, 90, 230, 210]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         "name" = Just "affiliate advertising"
         },
      special: barSeriesDefault {
        "stack" = Just "total",
        "data" = Just $ simpleData <$> [220, 182, 191, 234, 290, 330, 310]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         "name" = Just "video Ads"
         },
      special: barSeriesDefault {
        "stack" = Just "total",
        "data" = Just $ simpleData <$> [150, 232, 201, 154, 190, 330, 410]
        }
      },
    BarSeries {
      common: universalSeriesDefault {
         "name" = Just "search engine"
         },
      special: barSeriesDefault {
        "stack" = Just "total",
        "data" = Just $ simpleData <$> [820, 932, 901, 934, 1290, 1330, 1320]
        }
      }
    ]
  }
           
bind first second = do
  connect first second
  connect second first


connectM firstId secondId = do
  fst <- getElementById firstId
         >>= init Nothing
         >>= setOption options1 true

  snd <- getElementById secondId
         >>= init Nothing
         >>= setOption options2 true


  bind fst snd
