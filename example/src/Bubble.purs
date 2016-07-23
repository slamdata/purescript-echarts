module Bubble where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Tuple (Tuple(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

options ∷ E.Option
options =
  E.Option E.optionDefault
    { xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
      { "type" = Just E.ValueAxis
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
        { textStyle = Just $ E.TextStyle E.textStyleDefault
          { fontFamily = Just "Palatino, Georgia, serif"
          }
        }
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
        { textStyle = Just $ E.TextStyle E.textStyleDefault
          { fontFamily = Just "Palatino, Georgia, serif"
          }
        }
      }
    , tooltip = Just $ E.Tooltip E.tooltipDefault
      { trigger = Just E.TriggerAxis
      , textStyle = Just $ E.TextStyle E.textStyleDefault
        { fontFamily = Just "Palatino, Georgia, serif"
        , fontSize = Just 12.0
        }
      , axisPointer = Just $ E.TooltipAxisPointer E.tooltipAxisPointerDefault
        { "type" = Just E.CrossPointer
        , lineStyle = Just $ E.LineStyle E.lineStyleDefault
          { color = Just "rgba(170,170,170,0.8)"
          , width = Just 1.5
          , "type" = Just E.Solid
          }
        }
      }
    , series = Just $ map Just
      [ E.ScatterSeries
        { common: E.universalSeriesDefault
            { name = Just "A"}
        , scatterSeries: E.scatterSeriesDefault
          { large = Just true
          , "data" = Just $ map xyrData
              [Tuple (Tuple 10.0 20.0) Nothing]
          , symbol = Just E.Circle
          , symbolSize = Just $ E.ArrayMappingFunc (radiusMapper 0.0)
          }
        }
       , E.ScatterSeries
        { common: E.universalSeriesDefault
            { name = Just "B"}
        , scatterSeries: E.scatterSeriesDefault
          { large = Just true
          , "data" = Just $ map xyrData
              [Tuple (Tuple 5.0 10.0) (Just 20.0)]
          , symbol = Just E.Circle
          , symbolSize = Just $ E.ArrayMappingFunc (radiusMapper 0.0)
          }
        }
      ]
    }

xyrData ∷ Tuple (Tuple Number Number) (Maybe Number) → E.ItemData
xyrData (Tuple (Tuple x y) r)= E.Value $ E.XYR { x, y, r }


radiusMapper ∷ Number → (Array Number → Number)
radiusMapper i = func
  where
  func ∷ Array Number → Number
  func a = i + (fromMaybe 4.0 (a !! 2))

bubble
  ∷ ∀ eff
  . ElementId
  → Eff ( echarts ∷ E.ECHARTS, console ∷ CONSOLE, dom ∷ DOM |eff) Unit
bubble id = do
  mbEl ← U.getElementById id
  case mbEl  of
    Nothing → log "incorrect id in bubble"
    Just el → do
      chart ← E.init Nothing el
      chart' ← E.setOption options true chart
      pure unit
