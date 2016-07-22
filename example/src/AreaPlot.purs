module AreaPlot where

import DOM (DOM())
import DOM.Node.Types (ElementId(..))

import Prelude
import Control.Monad.Eff.Console (print, CONSOLE())
import Data.Tuple.Nested
import Data.Tuple
import Data.Maybe
import Data.Function
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
import ECharts.Style.Text
import ECharts.Style.Line
import ECharts.Style.Area
import ECharts.Color
import ECharts.Title
import ECharts.Symbol
import Control.Monad.Eff (Eff())
import ECharts.Utils
import ECharts.Effects


simpleData = Value <<< Simple


foreign import linearGradientColor :: LinearGradientInput -> LinearGradient


options :: Option
options = Option $ optionDefault {
  title = Just $ Title titleDefault {
    text = Just "Simple Area Plot",
    subtext = Just "color area, gradient color function, axis label formatter",
    x = Just XCenter,
    textStyle = Just $ TextStyle textStyleDefault {
      fontFamily = Just "Palatino, Georgia, serif"
      }
    },
  tooltip = Just $ Tooltip tooltipDefault {
    trigger = Just TriggerAxis,
    textStyle = Just $ TextStyle textStyleDefault {
      fontFamily = Just "Palatino, Georgia, serif",
      fontSize = Just 12.0 
      },
    axisPointer = Just $ TooltipAxisPointer tooltipAxisPointerDefault {
      "type" = Just $ LinePointer,
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(170,170,170,0.8)",
        width = Just 1.5,
        "type" = Just $ Solid
        }
      }
    },
  legend = Just $ Legend legendDefault {
    "data" = Just $ legendItemDefault <$>
             ["sample values"],
    y = Just YBottom,
    textStyle = Just $ TextStyle textStyleDefault {
      fontFamily = Just "Palatino, Georgia, serif"
      }
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just false,
    feature = Just $ Feature $ featureDefault 
    },
  calculable = Just true,
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    boundaryGap = Just $ CatBoundaryGap false,
    axisLine = Just $ AxisLine axisLineDefault {
      lineStyle = Just $ AxisLineStyle axisLineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    axisTick = Just $ AxisTick axisTickDefault {
      length = Just $ 2.0,
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    splitLine = Just $ AxisSplitLine axisSplitLineDefault {
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(204,204,204,0.2)",
        width = Just 1.0
        }
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      formatter = Just $ StringFormatFunc (dateTimeFormatter "MM-DD"),
      textStyle = Just $ TextStyle textStyleDefault {
        fontFamily = Just "Palatino, Georgia, serif"
        }
      },
    "data" = Just $ CommonAxisData <$>dataDate
    },
  yAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just ValueAxis,
    axisLine = Just $ AxisLine axisLineDefault {
      lineStyle = Just $ AxisLineStyle axisLineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    splitLine = Just $ AxisSplitLine axisSplitLineDefault {
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(204,204,204,0.2)",
        width = Just 1.0
        }
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      formatter = Just $ NumberFormatFunc (numeralFormatter "0.00a"),
      textStyle = Just $ TextStyle textStyleDefault {
        fontFamily = Just "Palatino, Georgia, serif"
        }
      }
    },

  series = Just $ Just <$> [
    LineSeries {
      common: universalSeriesDefault {
        name = Just "sample values",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "rgba(255,0,0,0.8)",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
                color = Just $ GradientColor (
                  linearGradientColor  $ linearGradientInputDefault {
                    x0 = 0.0, y0= 100.0, x1 = 0.0, y1 = 300.0, 
                    s0 = 0.0, sc0 = "rgba(255,0,0,0.8)", s1 = 1.0, sc1 = "rgba(255,255,255,0.1)"})           
              }
            }     
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ NoSymbol,
        smooth = Just true,
        "data" = Just $ simpleData <$> dataVal
        }
      } 
    ]
  }

areaPlot :: forall eff. 
  ElementId -> 
  Eff ( echartSetOption :: ECHARTS_OPTION_SET
        , echartInit :: ECHARTS_INIT
        , console :: CONSOLE
        , dom :: DOM
        | eff
        ) Unit
areaPlot id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in area-plot"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit

dataDate = 
  [ "2014-01-01", "2014-01-02", "2014-01-03", "2014-01-04", "2014-01-05", "2014-01-06", "2014-01-07"
  , "2014-01-08", "2014-01-09", "2014-01-10", "2014-01-11", "2014-01-12", "2014-01-13", "2014-01-14", "2014-01-15"
  , "2014-01-16", "2014-01-17", "2014-01-18", "2014-01-19", "2014-01-20", "2014-01-21", "2014-01-22", "2014-01-23"
  , "2014-01-24", "2014-01-25", "2014-01-26", "2014-01-27", "2014-01-28", "2014-01-29", "2014-01-30", "2014-01-31"
  , "2014-02-01", "2014-02-02", "2014-02-03", "2014-02-04", "2014-02-05", "2014-02-06", "2014-02-07", "2014-02-08"
  , "2014-02-09", "2014-02-10", "2014-02-11", "2014-02-12", "2014-02-13", "2014-02-14", "2014-02-15", "2014-02-16"
  , "2014-02-17", "2014-02-18", "2014-02-19", "2014-02-20", "2014-02-21", "2014-02-22", "2014-02-23", "2014-02-24"
  , "2014-02-25", "2014-02-26", "2014-02-27", "2014-02-28", "2014-03-01", "2014-03-02", "2014-03-03", "2014-03-04"
  , "2014-03-05", "2014-03-06", "2014-03-07", "2014-03-08", "2014-03-09", "2014-03-10", "2014-03-11", "2014-03-12"
  , "2014-03-13", "2014-03-14", "2014-03-15", "2014-03-16", "2014-03-17", "2014-03-18", "2014-03-19", "2014-03-20"
  , "2014-03-21", "2014-03-22", "2014-03-23", "2014-03-24", "2014-03-25", "2014-03-26", "2014-03-27", "2014-03-28"
  , "2014-03-29", "2014-03-30", "2014-03-31", "2014-04-01", "2014-04-02", "2014-04-03", "2014-04-04", "2014-04-05"
  , "2014-04-06", "2014-04-07", "2014-04-08", "2014-04-09", "2014-04-10", "2014-04-11", "2014-04-12", "2014-04-13"
  , "2014-04-14", "2014-04-15", "2014-04-16", "2014-04-17", "2014-04-18", "2014-04-19", "2014-04-20", "2014-04-21"
  , "2014-04-22", "2014-04-23", "2014-04-24", "2014-04-25", "2014-04-26", "2014-04-27", "2014-04-28", "2014-04-29"
  , "2014-04-30" ]
dataVal = 
  [ 190000000.0, 190379978.0, 90493749.0, 190785250.0, 197391904.0, 191576838.0, 191413854.0, 142177211.0
  , 103762210.0, 144381072.0, 154352310.0, 165531790.0, 175748881.0, 187064037.0, 197520685.0, 210176418.0, 196122924.0
  , 157337480.0, 200258882.0, 186829538.0, 112456897.0, 114299711.0, 122759017.0, 203596183.0, 208107346.0, 196359852.0
  , 192570783.0, 177967768.0, 190632803.0, 203725316.0, 118226177.0, 210698669.0, 217640656.0, 216142362.0, 201410971.0
  , 196704289.0, 190436945.0, 178891686.0, 171613962.0, 107579773.0, 158677098.0, 147129977.0, 151561876.0, 151627421.0
  , 143543872.0, 136581057.0, 135560715.0, 122625263.0, 112091484.0, 98810329.0, 99882912.0, 94943095.0, 104875743.0
  , 116383678.0, 105028841.0, 123967310.0, 133167029.0, 128577263.0, 115836969.0, 119264529.0, 109363374.0, 113985628.0
  , 114650999.0, 110866108.0, 96473454.0, 84075886.0, 103568384.0, 101534883.0, 115825447.0, 126133916.0, 116502109.0
  , 80169411.0, 84296886.0, 86347399.0, 31483669.0, 142811333.0, 89675396.0, 115514483.0, 117630630.0, 122340239.0
  , 132349091.0, 125613305.0, 135592466.0, 123408762.0, 111991454.0, 116123955.0, 112817214.0, 113029590.0, 108753398.0
  , 99383763.0, 100151737.0, 94985209.0, 82913669.0, 78748268.0, 63829135.0, 78694727.0, 80868994.0, 93799013.0, 9042416.0
  , 97298692.0, 53353499.0, 71248129.0, 75253744.0, 68976648.0, 71002284.0, 75052401.0, 83894030.0, 50236528.0, 59739114.0
  , 56407136.0, 108323177.0, 101578914.0, 115877608.0, 132088857.0, 112071353.0, 81790062.0, 105003761.0, 100457727.0
  , 118253926.0, 67956992.0 ]
