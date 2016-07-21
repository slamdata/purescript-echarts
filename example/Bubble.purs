module Bubble where

import DOM (DOM())
import DOM.Node.Types (ElementId)

import Prelude
import Control.Monad.Eff.Console (print, CONSOLE())
import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Array ((!!))
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)

import ECharts.Chart (init)
import ECharts.Tooltip (Tooltip(Tooltip), TooltipAxisPointer(TooltipAxisPointer), TooltipAxisPointerType(CrossPointer), TooltipTrigger(TriggerAxis), tooltipAxisPointerDefault, tooltipDefault)
import ECharts.Options (Option(Option), setOption, optionDefault)
import ECharts.Series (Series(ScatterSeries), scatterSeriesDefault, universalSeriesDefault)
import ECharts.Item.Value (ItemValue(XYR))
import ECharts.Item.Data (ItemData(Value))
import ECharts.Symbol (SymbolSize(ArrayMappingFunc), Symbol(Circle))
import ECharts.Axis (Axis(Axis), AxisLabel(AxisLabel), AxisLine(AxisLine), AxisLineStyle(AxisLineStyle), AxisSplitLine(AxisSplitLine), AxisTick(AxisTick), AxisType(ValueAxis), Axises(OneAxis), axisLabelDefault, axisSplitLineDefault, axisLineStyleDefault, axisLineDefault, axisDefault, axisTickDefault)
import ECharts.Style.Line (LineStyle(LineStyle), LineType(Solid), lineStyleDefault)
import ECharts.Style.Text (TextStyle(TextStyle), textStyleDefault)
import ECharts.Effects (ECHARTS_INIT, ECHARTS_OPTION_SET)
import Utils as U


options :: Option
options = Option $ optionDefault 
    { xAxis = Just $ OneAxis $ Axis $ axisDefault 
      { "type" = Just ValueAxis
      , axisLine = Just $ AxisLine axisLineDefault 
        { lineStyle = Just $ AxisLineStyle axisLineStyleDefault 
          { color = Just "rgba(184,184,184,0.8)"
          , width = Just 1.0
          }
        }
      , axisTick = Just $ AxisTick axisTickDefault 
        { length = Just $ 2.0
        , lineStyle = Just $ LineStyle lineStyleDefault 
          { color = Just "rgba(184,184,184,0.8)"
          , width = Just 1.0
          }
        }
      , splitLine = Just $ AxisSplitLine axisSplitLineDefault 
        { lineStyle = Just $ LineStyle lineStyleDefault 
          { color = Just "rgba(204,204,204,0.2)"
          , width = Just 1.0
          }
        }
      , axisLabel = Just $ AxisLabel axisLabelDefault 
        { textStyle = Just $ TextStyle textStyleDefault 
          { fontFamily = Just "Palatino, Georgia, serif"
          }
        }
      }
    , yAxis = Just $ OneAxis $ Axis $ axisDefault 
      { "type" = Just ValueAxis
      , axisLine = Just $ AxisLine axisLineDefault
        { lineStyle = Just $ AxisLineStyle axisLineStyleDefault 
          { color = Just "rgba(184,184,184,0.8)"
          , width = Just 1.0
          }
        }
      , splitLine = Just $ AxisSplitLine axisSplitLineDefault 
        { lineStyle = Just $ LineStyle lineStyleDefault 
          { color = Just "rgba(204,204,204,0.2)"
          , width = Just 1.0
          }
        }
      , axisLabel = Just $ AxisLabel axisLabelDefault 
        { textStyle = Just $ TextStyle textStyleDefault 
          { fontFamily = Just "Palatino, Georgia, serif"
          }
        }
      }
    , tooltip = Just $ Tooltip tooltipDefault 
      { trigger = Just TriggerAxis
      , textStyle = Just $ TextStyle textStyleDefault 
        { fontFamily = Just "Palatino, Georgia, serif"
        , fontSize = Just 12.0 
        }
      , axisPointer = Just $ TooltipAxisPointer tooltipAxisPointerDefault 
        { "type" = Just $ CrossPointer
        , lineStyle = Just $ LineStyle lineStyleDefault 
          { color = Just "rgba(170,170,170,0.8)"
          , width = Just 1.5
          , "type" = Just $ Solid
          }
        }
      }
    , series = Just $ Just <$> 
      [ ScatterSeries 
        { common: universalSeriesDefault
            { name = Just "A"}
        , scatterSeries: scatterSeriesDefault 
          { large = Just true
          , "data" = Just $ xyrData <$> 
              [Tuple (Tuple 10.0 20.0) Nothing]
          , symbol = Just Circle
          , symbolSize = Just $ ArrayMappingFunc (radiusMapper 0.0)
          }
        }
      , ScatterSeries 
        { common: universalSeriesDefault
            { name = Just "B"}
        , scatterSeries: scatterSeriesDefault 
          { large = Just true
          , "data" = Just $ xyrData <$> 
              [Tuple (Tuple 5.0 10.0) (Just 20.0)]
          , symbol = Just Circle
          , symbolSize = Just $ ArrayMappingFunc (radiusMapper 0.0)
          }
        }
      ]
    }

xyrData ∷ Tuple (Tuple Number Number) (Maybe Number) → ItemData
xyrData (Tuple a b)= Value $ XYR 
  { x: fst a
  , y: snd a
  , r: b
  }

radiusMapper ∷ Number → (Array Number → Number)
radiusMapper i = func
  where 
  func ∷ Array Number → Number
  func a = i + (fromMaybe 4.0 (a !! 2))

bubble :: forall eff. 
  ElementId -> 
  Eff ( echartSetOption :: ECHARTS_OPTION_SET
        , echartInit :: ECHARTS_INIT
        , console :: CONSOLE
        , dom :: DOM
        | eff
        ) Unit
bubble id = do
  mbEl <- U.getElementById id
  case mbEl  of
    Nothing -> print "incorrect id in bubble"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit

