module AreaPlot where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Formatter.Number as DFN
import Data.Formatter.DateTime as DFD

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

import Debug.Trace as DT

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

foreign import linearGradientColor
  ∷ E.LinearGradientInput → E.LinearGradient


options ∷ E.Option
options =
  E.Option E.optionDefault
    { title = Just $ E.Title E.titleDefault
        { text = Just "Simple Area Plot"
        , subtext = Just "color area, gradient color function, axis label formatter"
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
          { "type" = Just E.LinePointer
          , lineStyle = Just $ E.LineStyle E.lineStyleDefault
              { color = Just "rgba(170,170,170,0.8)"
              , width = Just 1.5
              , "type" = Just E.Solid
              }
          }
        }
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault ["sample values"]
        , y = Just E.YBottom
        , textStyle = Just $ E.TextStyle E.textStyleDefault
            { fontFamily = Just "Palatino, Georgia, serif" }
        }
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { show = Just false
        , feature = Just $ E.Feature E.featureDefault
        }
    , calculable = Just true
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
            { lineStyle = Just $ E.LineStyle E.lineStyleDefault
                { color = Just "rgba(204,204,204,0.2)"
                , width = Just 1.0
                }
            }
        , axisLabel = Just $ E.AxisLabel E.axisLabelDefault
            {
              formatter =
                 Just $ E.StringFormatFunc \s →
                   DT.spy $
                   either (const "Incorrect datetime") id
                   $ DFD.formatDateTime "MMM-DD"
                   =<< DFD.unformatDateTime "YYYY-MM-DD" s
            , textStyle = Just $ E.TextStyle E.textStyleDefault
                { fontFamily = Just "Palatino, Georgia, serif" }
            }
        , "data" = Just $ map E.CommonAxisData dataDate
    }
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.ValueAxis
        , axisLine = Just $ E.AxisLine E.axisLineDefault
            { lineStyle = Just $ E.AxisLineStyle E.axisLineStyleDefault
                { color = Just "rgba(184,184,184,0.8)"
                , width = Just 1.0
                }
            }
        , splitLine = Just $ E.AxisSplitLine E.axisSplitLineDefault
            { lineStyle = Just $ E.LineStyle E.lineStyleDefault
                { color = Just "rgba(204,204,204,0.2)"
                , width = Just 1.0
                }
            }
        , axisLabel = Just $ E.AxisLabel E.axisLabelDefault
--            { formatter = Just $ E.NumberFormatFunc (DFN.formatOrShowNumber "0.00a")
--            , textStyle = Just $ E.TextStyle E.textStyleDefault
--                { fontFamily = Just "Palatino, Georgia, serif" }
--            }
        }
    , series = Just $ map Just
        [ E.LineSeries
            { common: E.universalSeriesDefault
              { name = Just "sample values"
              , itemStyle = Just $ E.ItemStyle E.itemStyleDefault
                  { normal = Just $ E.IStyle E.istyleDefault
                     { color = Just $ E.SimpleColor "rgba(255,0,0,0.8)"
                     , areaStyle = Just $ E.AreaStyle E.areaStyleDefault
                        { color = Just $ E.GradientColor (
                             linearGradientColor $ E.linearGradientInputDefault
                             { x0 = 0.0
                             , y0= 100.0
                             , x1 = 0.0
                             , y1 = 300.0
                             , s0 = 0.0
                             , sc0 = "rgba(255,0,0,0.8)"
                             , s1 = 1.0
                             , sc1 = "rgba(255,255,255,0.1)"
                             })
                        }
                     }
                  }
              }
            , lineSeries: E.lineSeriesDefault
                { symbol = Just E.NoSymbol
                , smooth = Just true
                , "data" = Just $ map simpleData dataVal
                }
            }
        ]
    }

areaPlot
  ∷ ∀ eff
  . ElementId
  → Eff ( echarts ∷ E.ECHARTS, console ∷ CONSOLE, dom ∷ DOM | eff) Unit
areaPlot id = do
  mbEl ← U.getElementById id
  DT.traceAnyA "in are plot"
  case mbEl of
    Nothing → log "incorrect id in area-plot"
    Just el → do
      chart ← E.init Nothing el
      DT.traceAnyA "inited"
      DT.traceAnyA options
      E.setOption options true chart
      DT.traceAnyA "options are set"
      pure unit


dataDate ∷ Array String
dataDate =
  [ "2014-01-01", "2014-01-02", "2014-01-03", "2014-01-04"
  , "2014-01-05", "2014-01-06", "2014-01-07", "2014-01-08"
  , "2014-01-09", "2014-01-10", "2014-01-11", "2014-01-12"
  , "2014-01-13", "2014-01-14", "2014-01-15", "2014-01-16"
  , "2014-01-17", "2014-01-18", "2014-01-19", "2014-01-20"
  , "2014-01-21", "2014-01-22", "2014-01-23", "2014-01-24"
  , "2014-01-25", "2014-01-26", "2014-01-27", "2014-01-28"
  , "2014-01-29", "2014-01-30", "2014-01-31", "2014-02-01"
  , "2014-02-02", "2014-02-03", "2014-02-04", "2014-02-05"
  , "2014-02-06", "2014-02-07", "2014-02-08", "2014-02-09"
  , "2014-02-10", "2014-02-11", "2014-02-12", "2014-02-13"
  , "2014-02-14", "2014-02-15", "2014-02-16", "2014-02-17"
  , "2014-02-18", "2014-02-19", "2014-02-20", "2014-02-21"
  , "2014-02-22", "2014-02-23", "2014-02-24", "2014-02-25"
  , "2014-02-26", "2014-02-27", "2014-02-28", "2014-03-01"
  , "2014-03-02", "2014-03-03", "2014-03-04", "2014-03-05"
  , "2014-03-06", "2014-03-07", "2014-03-08", "2014-03-09"
  , "2014-03-10", "2014-03-11", "2014-03-12", "2014-03-13"
  , "2014-03-14", "2014-03-15", "2014-03-16", "2014-03-17"
  , "2014-03-18", "2014-03-19", "2014-03-20", "2014-03-21"
  , "2014-03-22", "2014-03-23", "2014-03-24", "2014-03-25"
  , "2014-03-26", "2014-03-27", "2014-03-28", "2014-03-29"
  , "2014-03-30", "2014-03-31", "2014-04-01", "2014-04-02"
  , "2014-04-03", "2014-04-04", "2014-04-05", "2014-04-06"
  , "2014-04-07", "2014-04-08", "2014-04-09", "2014-04-10"
  , "2014-04-11", "2014-04-12", "2014-04-13", "2014-04-14"
  , "2014-04-15", "2014-04-16", "2014-04-17", "2014-04-18"
  , "2014-04-19", "2014-04-20", "2014-04-21", "2014-04-22"
  , "2014-04-23", "2014-04-24", "2014-04-25", "2014-04-26"
  , "2014-04-27", "2014-04-28", "2014-04-29", "2014-04-30"
  ]

dataVal ∷ Array Number
dataVal =
  [ 190000000.0, 190379978.0, 90493749.0, 190785250.0
  , 197391904.0, 191576838.0, 191413854.0, 142177211.0
  , 103762210.0, 144381072.0, 154352310.0, 165531790.0
  , 175748881.0, 187064037.0, 197520685.0, 210176418.0
  , 196122924.0  , 157337480.0, 200258882.0, 186829538.0
  , 112456897.0, 114299711.0, 122759017.0, 203596183.0
  , 208107346.0, 196359852.0  , 192570783.0, 177967768.0
  , 190632803.0, 203725316.0, 118226177.0, 210698669.0
  , 217640656.0, 216142362.0, 201410971.0  , 196704289.0
  , 190436945.0, 178891686.0, 171613962.0, 107579773.0
  , 158677098.0, 147129977.0, 151561876.0, 151627421.0
  , 143543872.0, 136581057.0, 135560715.0, 122625263.0
  , 112091484.0, 98810329.0, 99882912.0, 94943095.0
  , 104875743.0  , 116383678.0, 105028841.0, 123967310.0
  , 133167029.0, 128577263.0, 115836969.0, 119264529.0
  , 109363374.0, 113985628.0  , 114650999.0, 110866108.0
  , 96473454.0, 84075886.0, 103568384.0, 101534883.0
  , 115825447.0, 126133916.0, 116502109.0  , 80169411.0
  , 84296886.0, 86347399.0, 31483669.0, 142811333.0
  , 89675396.0, 115514483.0, 117630630.0, 122340239.0
  , 132349091.0, 125613305.0, 135592466.0, 123408762.0
  , 111991454.0, 116123955.0, 112817214.0, 113029590.0
  , 108753398.0  , 99383763.0, 100151737.0, 94985209.0
  , 82913669.0, 78748268.0, 63829135.0, 78694727.0
  , 80868994.0, 93799013.0, 9042416.0  , 97298692.0
  , 53353499.0, 71248129.0, 75253744.0, 68976648.0
  , 71002284.0, 75052401.0, 83894030.0, 50236528.0
  , 59739114.0  , 56407136.0, 108323177.0, 101578914.0
  , 115877608.0, 132088857.0, 112071353.0, 81790062.0
  , 105003761.0, 100457727.0  , 118253926.0, 67956992.0
  ]
