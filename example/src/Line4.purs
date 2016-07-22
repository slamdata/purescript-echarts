module Line4 where

import Prelude
import Control.Monad.Eff.Console (print)
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
import Control.Monad.Eff (Eff())

simpleData = Value <<< Simple

options :: Option
options = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  legend = Just $ Legend legendDefault {
    x = Just XLeft,
    "data" = Just $ legendItemDefault <$>
             ["email marketing", "affiliate advertising",
              "video ads", "direct access", "search engine"]
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just true,
    x = Just XRight,
    y = Just YBottom,
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
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    boundaryGap = Just $ CatBoundaryGap false,
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
          name = Just "email marketing"
          },
       lineSeries: lineSeriesDefault {
         stack = Just "total",
         "data" = Just $ simpleData <$> [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
         }
       },

    LineSeries {
      common: universalSeriesDefault {
         name = Just "affiliate advertising"
         },
      lineSeries: lineSeriesDefault {
        "data" = Just $ simpleData <$> [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         name = Just "video ads"
         },
      lineSeries: lineSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         name = Just "direct access"
         },
      lineSeries: lineSeriesDefault {
        "data" = Just $ simpleData <$> [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0]
        }
      },

    LineSeries {
      common: universalSeriesDefault {
         name = Just "search engine"
         },
      lineSeries: lineSeriesDefault {
        stack = Just "total",
        "data" = Just $ simpleData <$> [820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0]
        }
      }
    ]

  }


line4 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in line4"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit

