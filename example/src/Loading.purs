module Loading where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.NonEmpty as NE

import DOM (DOM)
import DOM.Node.Types (ElementId)

import Utils as U

import ECharts as E

import Signal (runSignal, (~>), Signal, foldp)
import Signal.Time as ST

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

allEffects ∷ NE.NonEmpty Array E.LoadingEffect
allEffects = E.Spin NE.:| [ E.Bar, E.Ring, E.Whirling, E.DynamicLine, E.Bubble]

effect ∷ E.LoadingEffect → E.LoadingOption
effect eff =
  E.LoadingOption E.loadingOptionDefault
    { text = Just $ "effect"
    , effect = Just eff
    , textStyle = Just $ E.TextStyle E.textStyleDefault {fontSize = Just 20.0}
    }

series ∷ Boolean → Array E.Series
series true =
  [ E.LineSeries
      { common: E.universalSeriesDefault
          { name = Just "first" }
      , lineSeries: E.lineSeriesDefault
          { "data" = Just $ map simpleData
                       [2.0, 4.9, 7.0, 23.2, 25.6, 76.7, 135.6, 162.2, 32.6, 20.0, 6.4, 3.3]
          }
      }
  , E.LineSeries
      { common: E.universalSeriesDefault
          { name = Just "second" }
      , lineSeries: E.lineSeriesDefault
          { "data" = Just $ map simpleData
                       [2.6, 5.9, 9.0, 26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
          }
      }
  ]
series false =
  [ E.BarSeries
      { common: E.universalSeriesDefault
          { name = Just "first" }
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData
                       [2.0, 4.9, 7.0, 23.2, 25.6, 76.7, 135.6, 162.2, 32.6, 20.0, 6.4, 3.3]
          }
      }
  , E.BarSeries
      { common: E.universalSeriesDefault
          { name = Just "second" }
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData
                       [2.6, 5.9, 9.0, 26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
          }
      }
  ]


options ∷ Int → E.Option
options i =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerAxis}
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { show = Just true
        , feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature $ E.markFeatureDefault {show = Just true}
            , dataView = Just $ E.DataViewFeature E.dataViewFeatureDefault
                { show = Just true
                , readOnly = Just false
                }
            , magicType = Just $ E.MagicTypeFeature E.magicTypeFeatureDefault
                { show = Just true
                , "type" = Just [E.MagicLine, E.MagicBar]
                }
            , restore = Just $ E.RestoreFeature E.restoreFeatureDefault
                { show = Just true }
            , saveAsImage = Just $ E.SaveAsImageFeature E.saveAsImageFeatureDefault
                { show = Just true }
            }
        }
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault ["first","second"] }
    , xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.CategoryAxis
        , "data" = Just $ map E.CommonAxisData
                     ["1","2","3","4","5","6","7","8","9","10","11","12"]
        }
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.ValueAxis }
    , series = Just $ map Just (series (i `mod` 2 == 0))
  }

data ChartSignal a
  = StartLoading E.LoadingOption
  | StopLoading a


dataStream
  ∷ ∀ eff
  . Signal (Eff (random ∷ RANDOM | eff) (ChartSignal E.Option))
dataStream =
  foldp (\_ curstateE → do
          curstate ← curstateE
          case curstate of
            StartLoading _ → do
              effAndI ← U.randomInArray allEffects
              case effAndI of
                Tuple eff i →
                  pure $ StopLoading $ options i
            StopLoading _ → do
              effAndI ← U.randomInArray allEffects
              case effAndI of
                Tuple eff i →
                  pure $ StartLoading (effect eff))
  (pure $ StartLoading (effect E.Spin))
  (ST.every 2000.0)

loading
  ∷ ∀ e
  . ElementId
  → Eff (dom ∷ DOM, echarts ∷ E.ECHARTS, random ∷ RANDOM, console ∷ CONSOLE|e) Unit
loading id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in loading"
    Just el → do
      chart ← E.init Nothing el
      runSignal $ dataStream ~> \effContent → do
        content ← effContent
        case content of
          StartLoading loadOptions →
            E.showLoading loadOptions chart
              >>= \_ → pure unit
          StopLoading options' →
            E.setOption options' true chart
              >>= E.hideLoading
              >>= \_ → pure unit
