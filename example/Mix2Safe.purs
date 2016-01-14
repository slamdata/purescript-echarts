module Mix2Safe where

import Prelude
import Control.Monad.Eff.Console (print)
import Control.Monad.Eff
import Utils

import Data.Maybe
import Data.Tuple

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
series :: Array Series
series = [
  BarSeries {
     common: universalSeriesDefault {name = Just "direct access"},
     barSeries: barSeriesDefault {
       "data" = Just $ simpleData <$> [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0],
       stack = Just "total"
       }
     },
  BarSeries {
    common: universalSeriesDefault {
       name = Just "email marketing",
       tooltip = Just $ Tooltip $ tooltipDefault {trigger = Just TriggerItem}
       },
    barSeries: barSeriesDefault {
      "data" = Just $ simpleData <$> [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0],
      stack = Just "total"
      }
    },
  BarSeries {
    common: universalSeriesDefault {
       name = Just "affiliate advertising",
       tooltip = Just $ Tooltip $ tooltipDefault {trigger = Just TriggerItem}
       },
    barSeries: barSeriesDefault {
      "data" = Just $ simpleData <$> [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
      }
    },
  BarSeries {
    common: universalSeriesDefault {
       name = Just "video ads",
       tooltip = Just $ Tooltip $ tooltipDefault {trigger = Just TriggerItem}
       },
    barSeries: barSeriesDefault {
      "data" = Just $ simpleData <$> [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
      }
    },
  LineSeries {
    common: universalSeriesDefault{
       name = Just "must be"
       },
    lineSeries: lineSeriesDefault {
      "data" = Just $ simpleData <$>
               [862.0, 1018.0, 964.0, 1026.0, 1679.0, 1600.0, 1570.0]
      }
    },
  PieSeries {
    common: universalSeriesDefault {
       name = Just "search engine",
       itemStyle = Just $ ItemStyle {
         emphasis: Nothing,
         normal: Just $ IStyle $ istyleDefault {
           labelLine = Just $ ItemLabelLine {
              show: Just true,
              length: Just 20.0,
              lineStyle: Nothing
              }
           }
         },
       tooltip = Just $ Tooltip $ tooltipDefault {
         trigger = Just TriggerItem,
         formatter = Just $ Template "{a} <br/> {b}: {c} ({d}%)"
         }
       },
    pieSeries: pieSeriesDefault {
      radius = Just $ Rs {inner: (Pixel 0.0), outer: (Pixel 50.0)},
      center = Just $ Tuple (Percent 160.0) (Percent 130.0),
      "data" = Just $ Dat <$> [
         (dataDefault (Simple 1047.0)) {
            name = Just "Baidu"
            },
         (dataDefault (Simple 264.0)) {
           name = Just "Google"
           },
         (dataDefault (Simple 145.0)) {
           name = Just "Bing"
           },
         (dataDefault (Simple 102.0)) {
           name = Just "other"
           }
         ]


      }
    }

  ]


options :: Option
options = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  toolbox = Just $ Toolbox toolboxDefault {
    show = Just true,
    y = Just YBottom,
    feature = Just $ Feature {
      mark: Just $ MarkFeature {
         show: Just true,
         title: Nothing,
         lineStyle: Nothing
         },
      dataView: Just $ DataViewFeature {
        show: Just true,
        title: Nothing,
        readOnly: Just true,
        lang: Nothing
        },
      magicType: Just $ MagicTypeFeature {
        show: Just true,
        title: Nothing,
        option: Nothing,
        "type": Just $ [MagicLine, MagicBar, MagicStack, MagicTiled]
        },
      restore: Just $ RestoreFeature {
        show: Just true,
        title: Nothing
        },
      saveAsImage: Just $ SaveAsImageFeature {
        show: Just true,
        title: Nothing,
        "type": Nothing,
        lang: Nothing
        },
      dataZoom: Nothing
      }
    },
  calculable = Just true,
  legend = Just $ Legend $ legendDefault {
    "data" = Just $ legendItemDefault <$>
             ["direct access",
              "email marketing",
              "affiliate advertising",
              "video ads",
              "search engine",
              "Baidu",
              "Google",
              "must be",
              "other"]

    },
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    splitLine = Just $ AxisSplitLine $ axisSplitLineDefault {
      show = Just false
      },
    "data" = Just $ CommonAxisData <$>
             ["Monday", "Tuesday", "Wednesday",
              "Thursday", "Friday", "Saturday", "Sunday"]
     },
   yAxis = Just $ OneAxis $ Axis $ axisDefault {
     "type" = Just ValueAxis,
     position = Just RightAxis
     },
   series = Just $ Just <$> series

   }


mix2safe id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in mix2safe"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit




