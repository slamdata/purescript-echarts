module Line4 where

import Debug.Trace (trace)
import Data.Tuple.Nested
import Data.Maybe
import Utils

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

simpleData = Value <<< Simple

options :: Option
options = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  legend = Just $ Legend legendDefault {
    "x" = Just XLeft,
    "data" = Just $ legendItemDefault <$>
             ["email marketing", "affiliate advertising",
              "video ads", "direct access", "search engine"]
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
  calculable = Just true,
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    "boundaryGap" = Just $ CatBoundaryGap false,
    "data" = Just $ CommonAxisData <$>
             ["Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday"]
    },
  yAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just ValueAxis
    },

  series = Just $ Just <$> [
    LineSeries {
       common: universalSeriesDefault {
          "name" = Just "email marketing"
          },
       lineSeries: lineSeriesDefault {
         "stack" = Just "total",
         "data" = Just $ simpleData <$> [120, 132, 101, 134, 90, 230, 210]
         }
       },

    LineSeries {
      common: universalSeriesDefault {
         "name" = Just "affiliate advertising"
         },
      lineSeries: lineSeriesDefault {
        "data" = Just $ simpleData <$> [220, 182, 191, 234, 290, 330, 310]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         "name" = Just "video ads"
         },
      lineSeries: lineSeriesDefault {
        "stack" = Just "total",
        "data" = Just $ simpleData <$> [150, 232, 201, 154, 190, 330, 410]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         "name" = Just "direct access"
         },
      lineSeries: lineSeriesDefault {
        "data" = Just $ simpleData <$> [320, 332, 301, 334, 390, 330, 320]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         "name" = Just "search engine"
         },
      lineSeries: lineSeriesDefault {
        "stack" = Just "total",
        "data" = Just $ simpleData <$> [820, 932, 901, 934, 1290, 1330, 1320]
        }
      }
    ]

  }


line4 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> trace "incorrect id in line4"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit

