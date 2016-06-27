module PercentageArea where

import Prelude
import Control.Monad.Eff.Console (print)
import Data.Tuple.Nested
import Data.Tuple
import Data.Maybe
import Data.Array (zipWith, head, tail, cons)
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

foreign import anotationFomatter :: forall eff. (Eff eff Unit)

mkAnotationItem :: ItemValue -> ItemDataDatRec
mkAnotationItem value =
  {
    value: value,
    name: Nothing,
    tooltip: Nothing,
    itemStyle: 
      Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            label = Just $ ItemLabel itemLabelDefault { 
              show = Just $ true,
              position = Just LPRight,
              formatter = Just $ Template "\n{a}",
              textStyle = Just $ TextStyle textStyleDefault {
                fontFamily = Just "Palatino, Georgia, serif",
                fontSize = Just 10.0,
                color = Just "rgba(255,255,255,1)",
                align = Just HAlignRight,
                baseline = Just TBLBottom
                }       
              }  
            }   
          },
    selected: Nothing
  }

fromJustArray :: forall a. Maybe (Array a) -> Array a
fromJustArray arr = do
  case arr of
    Nothing -> []
    Just arr ->  arr

fromJustNumber :: Maybe Number -> Number
fromJustNumber n = do
  case n of
    Nothing -> 0.0
    Just n ->  n


options :: Option
options = Option $ optionDefault {
  title = Just $ Title titleDefault {
    text = Just "Percentage Area",
    subtext = Just "percentage area",
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
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just false,
    feature = Just $ Feature $ featureDefault 
    },
  calculable = Just false,
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
      show = Just false
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      textStyle = Just $ TextStyle textStyleDefault {
        fontFamily = Just "Palatino, Georgia, serif"
        }
      },
    "data" = Just $ CommonAxisData <$> year
    },
  yAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just ValueAxis,
    min = Just 0.0,
    max = Just 100.0,
    axisLine = Just $ AxisLine axisLineDefault {
      lineStyle = Just $ AxisLineStyle axisLineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    splitLine = Just $ AxisSplitLine axisSplitLineDefault {
      show = Just false
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      textStyle = Just $ TextStyle textStyleDefault {
       fontFamily = Just "Palatino, Georgia, serif"
       }
      }
    },

  series = Just $ Just <$> [
    LineSeries {
      common: universalSeriesDefault {
        name = Just "age under 30",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "#99B898",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
              color = Just $ SimpleColor "#99B898"           
              }  
            }   
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ Circle,
        symbolSize = Just $ Size 0.0,
        smooth = Just true,
        "data" = Just $ (cons
          (Dat $ mkAnotationItem (Simple (fromJustNumber $ head age_under_30)))
          (simpleData <$> (fromJustArray $ tail age_under_30))
          ),
        stack = Just $ "percentage"
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "age 30-39",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "#FECEA8",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
              color = Just $ SimpleColor "#FECEA8"           
              }    
            }   
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ Circle,
        symbolSize = Just $ Size 0.0,
        smooth = Just true,
        "data" = Just $ (cons
          (Dat $ mkAnotationItem (Simple (fromJustNumber $ head age_30_39)))
          (simpleData <$> (fromJustArray $ tail age_30_39))
          ),
        stack = Just $ "percentage"
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "age 40-49",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "#FF847C",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
              color = Just $ SimpleColor "#FF847C"           
              }  
            }   
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ Circle,
        symbolSize = Just $ Size 0.0,
        smooth = Just true,
        "data" = Just $ (cons
          (Dat $ mkAnotationItem (Simple (fromJustNumber $ head age_40_49)))
          (simpleData <$> (fromJustArray $ tail age_40_49))
          ),
        stack = Just $ "percentage"
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "age 50-59",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "#E84A5F",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
              color = Just $ SimpleColor "#E84A5F"           
              }  
            }   
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ Circle,
        symbolSize = Just $ Size 0.0,
        smooth = Just true,
        "data" = Just $ (cons
          (Dat $ mkAnotationItem (Simple (fromJustNumber $ head age_50_59)))
          (simpleData <$> (fromJustArray $ tail age_50_59))
          ),
        stack = Just $ "percentage"
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "age 60-64",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "#2A363B",
            areaStyle = Just $ AreaStyle areaStyleDefault { 
              color = Just $ SimpleColor "#2A363B"           
              }  
            }   
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ Circle,
        symbolSize = Just $ Size 0.0,
        smooth = Just true,
        "data" = Just $ (cons
          (Dat $ mkAnotationItem (Simple (fromJustNumber $ head age_60_64)))
          (simpleData <$> (fromJustArray $ tail age_60_64))
          ),
        stack = Just $ "percentage"
        }
      }
    ]
  }


percentageArea id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in percentage-area"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit


year = ["1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", 
  "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996",
  "1997", "1998", "1999", "2000", "2001", "2002", "2003"]

age_under_30 =[6.04, 5.69, 5.36, 4.95, 6.23, 7.2, 6.94, 7.11, 7.33, 7.65, 8.02, 8.36, 8.29, 9.35, 8.9, 
  8.68, 10.14, 8.73, 8.44, 8.38, 10.03, 9.13, 9.15, 8.56, 7.02, 6.87, 6.21, 6.26, 5.89, 5.97, 6.34, 7.68, 
  7.62, 6.91]

age_30_39 =  [7.25, 7.22, 6.92, 6.88, 7.52, 8.2, 8.37, 8.87, 9.16, 9.17, 9.7, 10.2, 10.56, 11.85, 12.9, 13.18, 
  15.37, 15.71, 15.3, 15.58, 16.69, 17.37, 18.19, 18.38, 17.82, 16.47, 15.72, 15.36, 14.56, 13.75, 13.28, 13.56, 
  12.49, 12.23]

age_40_49 =[16.72, 16.31, 15.92, 15.47, 15.55, 15.85, 15.6, 15.5, 15.58, 14.75, 14.8, 15.03, 15.2, 15.57, 16.34, 
  17.48, 18.63, 18.39, 19.07, 20.63, 20.37, 21.86, 22.77, 22.75, 23.68, 23.45, 24.53, 25.19, 24.93, 25.53, 24.52, 
  23.83, 24.08, 23.98]

age_50_59 = [39.89, 39.99, 40.69, 41.91, 40.81, 40.16, 39.89, 39.92, 39.99, 40.31, 40.07, 39.62, 39.28, 37.39, 
  36.47, 38.2, 35.78, 36.27, 37.24, 34.88, 34.93, 33.69, 32.19, 34.53, 35.86, 36.77, 37.81, 37.83, 39.7, 39.43, 
  39.46, 39.36, 40.13, 40.62]

age_60_64 = [30.09, 30.8, 31.12, 30.79, 29.89, 28.58, 29.21, 28.6, 27.94, 28.12, 27.41, 26.8, 26.67, 25.83, 25.4, 
  22.47, 20.08, 20.9, 19.95, 20.52, 17.97, 17.96, 17.7, 15.77, 15.62, 16.45, 15.74, 15.36, 14.92, 15.32, 16.4, 
  15.56, 15.67, 16.27]  