module AreaPlot where

import Prelude
import Control.Monad.Eff.Console (print)
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

simpleData = Value <<< Simple

foreign import linearGradientColor :: forall eff. 
  Fn8 Number Number Number Number Number String Number String (Eff eff Unit)

linearGradientColorCurried :: forall eff. 
  Number -> Number -> Number -> Number -> 
  Number -> String -> Number -> String -> Eff eff Unit
linearGradientColorCurried = runFn8 linearGradientColor

options :: Option
options = Option $ optionDefault {
  title = Just $ Title titleDefault {
    text = Just "Area Plot",
    subtext = Just "based on purescript & echarts"
    },
  tooltip = Just $ Tooltip tooltipDefault {
    trigger = Just TriggerAxis,
    textStyle = Just $ TextStyle textStyleDefault {
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
             ["value", "another value"]
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
    "data" = Just $ CommonAxisData <$>
             ["2014-01-01", "2014-01-02", "2014-01-03",
              "2014-01-04", "2014-01-05", "2014-01-06", 
              "2014-01-07", "2014-01-08", "2014-01-09"]
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
      }
    },

  series = Just $ Just <$> [
    LineSeries {
      common: universalSeriesDefault {
        name = Just "value",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor  "rgba(204,204,204,0.2)",
            areaStyle = Just $ AreaStyle areaStyleDefault
            }     
          }
        },
      lineSeries: lineSeriesDefault {
        smooth = Just true,
        symbol = Just $ NoSymbol,
        "data" = Just $ simpleData <$> [120.0, 132.0, 101.0, 134.0, 90.0, 180.0, 210.0, 190.0, 180.0]
        }
      } 
    ,
    LineSeries {
      common: universalSeriesDefault {
        name = Just "another value",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "rgba(255,0,0,0.8)",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
                color = Just $ ForeignColorFunc (
                  linearGradientColorCurried 
                    0.0 200.0 0.0 600.0 
                    0.0 "rgba(255,0,0,0.8)" 1.0 "rgba(255,255,255,0.1)")             
              }
            }     
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ NoSymbol,
        smooth = Just true,
        "data" = Just $ simpleData <$> [20.0, 82.0, 91.0, 34.0, 120.0, 30.0, 10.0, 90.0, 50.0]
        }
      }
  
    ]

  }


areaPlot id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in area-plot"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit

