module PercentageArea where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE())

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (head, tail, cons)

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

mkAnnotationItem ∷ E.ItemValue → E.ItemDataDatRec
mkAnnotationItem value =
  { value: value
  , name: Nothing
  , tooltip: Nothing
  , selected: Nothing
  , itemStyle:
      Just $ E.ItemStyle E.itemStyleDefault
        { normal =
            Just $ E.IStyle E.istyleDefault
              { label =
                  Just $ E.ItemLabel E.itemLabelDefault
                    { show = Just $ true
                    , position = Just E.LPRight
                    , formatter = Just $ E.Template "\n{a}"
                    , textStyle =
                        Just $ E.TextStyle E.textStyleDefault
                          { fontFamily = Just "Palatino, Georgia, serif"
                          , fontSize = Just 10.0
                          , color = Just "rgba(255,255,255,1)"
                          , align = Just E.HAlignRight
                          , baseline = Just E.TBLBottom
                          }
                    }
              }
        }
  }

options ∷ E.Option
options =
  E.Option $ E.optionDefault
    { title = Just $ E.Title E.titleDefault
        { text = Just "Percentage Area"
        , subtext = Just "stacked areas, series annotation"
        , x = Just E.XCenter
        , textStyle = Just $ E.TextStyle E.textStyleDefault
            { fontFamily = Just "Palatino, Georgia, serif" }
        }
    , tooltip = Just $ E.Tooltip E.tooltipDefault
        { trigger = Just E.TriggerAxis
        , textStyle = Just $ E.TextStyle E.textStyleDefault
            { fontFamily = Just "Palatino, Georgia, serif"
            , fontSize = Just 12.0
            }
        , axisPointer = Just $ E.TooltipAxisPointer E.tooltipAxisPointerDefault
            { "type" = Just $ E.LinePointer
            , lineStyle = Just $ E.LineStyle E.lineStyleDefault
                { color = Just "rgba(170,170,170,0.8)"
                , width = Just 1.5
                , "type" = Just E.Solid
                }
            }
        }
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { show = Just false
        , feature = Just $ E.Feature E.featureDefault
        }
    , calculable = Just false
    , xAxis = Just $ E.OneAxis $ E.Axis $ E.axisDefault
        { "type" = Just E.CategoryAxis
        , boundaryGap = Just $ E.CatBoundaryGap false
        , axisLine = Just $ E.AxisLine E.axisLineDefault
            { lineStyle = Just $ E.AxisLineStyle E.axisLineStyleDefault
                { color = Just "rgba(184,184,184,0.8)"
                , width = Just 1.0
                }
            }
        , axisTick = Just $ E.AxisTick E.axisTickDefault
            { length = Just $ 2.0
            , lineStyle = Just $ E.LineStyle E.lineStyleDefault
                { color = Just "rgba(184,184,184,0.8)"
                , width = Just 1.0
                }
            }
        , splitLine = Just $ E.AxisSplitLine E.axisSplitLineDefault
            { show = Just false }
        , axisLabel = Just $ E.AxisLabel E.axisLabelDefault
            { textStyle = Just $ E.TextStyle E.textStyleDefault
                { fontFamily = Just "Palatino, Georgia, serif" }
            }
        , "data" = Just $ map E.CommonAxisData year
    }
    , yAxis = Just $ E.OneAxis $ E.Axis $ E.axisDefault
        { "type" = Just E.ValueAxis
        , min = Just 0.0
        , max = Just 100.0
        , axisLine = Just $ E.AxisLine E.axisLineDefault
            { lineStyle = Just $ E.AxisLineStyle E.axisLineStyleDefault
                { color = Just "rgba(184,184,184,0.8)"
                , width = Just 1.0
                }
            }
        , splitLine = Just $ E.AxisSplitLine E.axisSplitLineDefault
            { show = Just false }
        , axisLabel = Just $ E.AxisLabel E.axisLabelDefault
            { textStyle = Just $ E.TextStyle E.textStyleDefault
                { fontFamily = Just "Palatino, Georgia, serif" }
            }
        }
    , series = Just $ map Just
      [ E.LineSeries
          { common: E.universalSeriesDefault
            { name = Just "age under 30"
            , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                { normal = Just $ E.IStyle E.istyleDefault
                    { color = Just $ E.SimpleColor "#99B898"
                    , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                        { color = Just $ E.SimpleColor "#99B898" }
                    }
                }
            }
          , lineSeries: E.lineSeriesDefault
              { symbol = Just $ E.Circle
              , symbolSize = Just $ E.Size 0.0
              , smooth = Just true
              , "data" =
                  Just $ (cons (E.Dat $ mkAnnotationItem (E.Simple (fromMaybe zero $ head ageUnder30)))
                          (simpleData <$> (fromMaybe [] $ tail ageUnder30))
                         )
              , stack = Just $ "percentage"
              }
          }
      , E.LineSeries
          { common: E.universalSeriesDefault
              { name = Just "age 30-39"
              , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                  { normal = Just $ E.IStyle E.istyleDefault
                      { color = Just $ E.SimpleColor "#FECEA8"
                      , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                          { color = Just $ E.SimpleColor "#FECEA8" }
                      }
                  }
              }
          , lineSeries: E.lineSeriesDefault
              { symbol = Just $ E.Circle
              , symbolSize = Just $ E.Size 0.0
              , smooth = Just true
              , "data" =
                  Just $ (cons (E.Dat $ mkAnnotationItem (E.Simple (fromMaybe zero $ head age30To39)))
                          (simpleData <$> (fromMaybe [] $ tail age30To39))
                         )
              , stack = Just $ "percentage"
              }
          }
      , E.LineSeries
          { common: E.universalSeriesDefault
            { name = Just "age 40-49"
            , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                { normal = Just $ E.IStyle E.istyleDefault
                    { color = Just $ E.SimpleColor "#FF847C"
                    , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                        { color = Just $ E.SimpleColor "#FF847C" }
                    }
                }
            }
          , lineSeries: E.lineSeriesDefault
              { symbol = Just E.Circle
              , symbolSize = Just $ E.Size 0.0
              , smooth = Just true
              , "data" =
                  Just $ (cons (E.Dat $ mkAnnotationItem (E.Simple (fromMaybe zero $ head age40To49)))
                          (simpleData <$> (fromMaybe [] $ tail age40To49))
                         )
              , stack = Just $ "percentage"
              }
          }
      , E.LineSeries
          { common: E.universalSeriesDefault
              { name = Just "age 50-59"
              , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                  { normal = Just $ E.IStyle E.istyleDefault
                      { color = Just $ E.SimpleColor "#E84A5F"
                      , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                          { color = Just $ E.SimpleColor "#E84A5F" }

                      }
                  }
              }
          , lineSeries: E.lineSeriesDefault
              { symbol = Just E.Circle
              , symbolSize = Just $ E.Size 0.0
              , smooth = Just true
              , "data" =
                  Just $ (cons (E.Dat $ mkAnnotationItem (E.Simple (fromMaybe zero $ head age50To59)))
                          (simpleData <$> (fromMaybe [] $ tail age50To59))
                         )
              , stack = Just $ "percentage"
              }
          }
      , E.LineSeries
          { common: E.universalSeriesDefault
              { name = Just "age 60-64"
              , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                  { normal = Just $ E.IStyle E.istyleDefault
                      { color = Just $ E.SimpleColor "#2A363B"
                      , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                          { color = Just $ E.SimpleColor "#2A363B" }
                      }
                  }
              }

          , lineSeries: E.lineSeriesDefault
              { symbol = Just E.Circle
              , symbolSize = Just $ E.Size 0.0
              , smooth = Just true
              , "data" =
                  Just $ (cons (E.Dat $ mkAnnotationItem (E.Simple (fromMaybe zero $ head age60To64)))
                          (simpleData <$> (fromMaybe [] $ tail age60To64))
                         )
              , stack = Just $ "percentage"
              }
          }
      ]
    }

percentageArea
  ∷ ∀ eff
  . ElementId
  → Eff (echarts ∷ E.ECHARTS, console ∷ CONSOLE, dom ∷ DOM|eff) Unit
percentageArea id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in percentage-area"
    Just el → do
      chart ← E.init Nothing el
      chart' ← E.setOption options true chart
      pure unit

year ∷ Array String
year =
  [ "1970"
  , "1971"
  , "1972"
  , "1973"
  , "1974"
  , "1975"
  , "1976"
  , "1977"
  , "1978"
  , "1979"
  , "1980"
  , "1981"
  , "1982"
  , "1983"
  , "1984"
  , "1985"
  , "1986"
  , "1987"
  , "1988"
  , "1989"
  , "1990"
  , "1991"
  , "1992"
  , "1993"
  , "1994"
  , "1995"
  , "1996"
  , "1997"
  , "1998"
  , "1999"
  , "2000"
  , "2001"
  , "2002"
  , "2003"
  ]

ageUnder30 ∷ Array Number
ageUnder30 =
  [ 6.04, 5.69, 5.36, 4.95, 6.23
  , 7.2, 6.94, 7.11, 7.33, 7.65
  , 8.02, 8.36, 8.29, 9.35, 8.9
  , 8.68, 10.14, 8.73, 8.44, 8.38
  , 10.03, 9.13, 9.15, 8.56, 7.02
  , 6.87, 6.21, 6.26, 5.89, 5.97
  , 6.34, 7.68, 7.62, 6.91
  ]

age30To39 ∷ Array Number
age30To39 =
  [ 7.25, 7.22, 6.92, 6.88, 7.52
  , 8.2, 8.37, 8.87, 9.16, 9.17
  , 9.7, 10.2, 10.56, 11.85, 12.9
  , 13.18, 15.37, 15.71, 15.3, 15.58
  , 16.69, 17.37, 18.19, 18.38, 17.82
  , 16.47, 15.72, 15.36, 14.56, 13.75
  , 13.28, 13.56, 12.49, 12.23 ]

age40To49 ∷ Array Number
age40To49 =
  [ 16.72, 16.31, 15.92, 15.47, 15.55
  , 15.85, 15.6, 15.5, 15.58, 14.75
  , 14.8, 15.03, 15.2, 15.57, 16.34
  , 17.48, 18.63, 18.39, 19.07, 20.63
  , 20.37, 21.86, 22.77, 22.75, 23.68
  , 23.45, 24.53, 25.19, 24.93
  , 25.53, 24.52, 23.83, 24.08, 23.98 ]

age50To59 ∷ Array Number
age50To59 =
  [ 39.89, 39.99, 40.69, 41.91, 40.81
  , 40.16, 39.89, 39.92, 39.99, 40.31
  , 40.07, 39.62, 39.28, 37.39, 36.47
  , 38.2, 35.78, 36.27, 37.24, 34.88
  , 34.93, 33.69, 32.19, 34.53, 35.86
  , 36.77, 37.81, 37.83
  , 39.7, 39.43, 39.46, 39.36, 40.13, 40.62 ]

age60To64 ∷ Array Number
age60To64 =
  [ 30.09, 30.8, 31.12, 30.79, 29.89
  , 28.58, 29.21, 28.6, 27.94, 28.12
  , 27.41, 26.8, 26.67, 25.83, 25.4
  , 22.47, 20.08, 20.9, 19.95, 20.52
  , 17.97, 17.96, 17.7, 15.77, 15.62
  , 16.45, 15.74, 15.36, 14.92, 15.32
  , 16.4, 15.56, 15.67, 16.27 ]
