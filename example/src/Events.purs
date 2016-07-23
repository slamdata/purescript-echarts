module Events where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE

import Debug.Trace as DT

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Math (round)

import Utils as U

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

lineData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array Number)
lineData = do
  lst ← map NE.oneOf $ U.randomArray 30
  pure $ (\x → round $ x * 30.0 + 30.0 ) <$> lst

barData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array Number)
barData = do
  lst ← map NE.oneOf $ U.randomArray 30
  pure $ (\x → round $ x * 10.0) <$> lst

options_ ∷ Array Number → Array Number → E.Option
options_ line bar =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerAxis}
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault ["fst","snd"] }
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { show = Just true
        , x = Just E.XRight
        , feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature E.markFeatureDefault {show = Just true}
            , dataView = Just $ E.DataViewFeature E. dataViewFeatureDefault
                { show = Just true
                , readOnly = Just false
                }
            , magicType = Just $ E.MagicTypeFeature E.magicTypeFeatureDefault
                { show = Just true
                , "type" = Just [E.MagicLine, E.MagicBar, E.MagicStack, E.MagicTiled]
                }
            , restore = Just $ E.RestoreFeature E.restoreFeatureDefault
                { show = Just true }
            , saveAsImage = Just $ E.SaveAsImageFeature E.saveAsImageFeatureDefault
                { show = Just true }
            }
        }
    , calculable = Just true
    , dataZoom = Just $ E.DataZoom $ E.dataZoomDefault
        { show = Just true
        , realtime = Just true
        , start = Just 40.0
        , end = Just 60.0
        }
    , xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.CategoryAxis
        , boundaryGap = Just $ E.CatBoundaryGap true
        , "data" = Just $ (\i → E.CommonAxisData $ "2013-03-" <> show i) <$> (1..30)
        }
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault {"type" = Just E.ValueAxis}
    , series = Just $ map Just
        [ E.LineSeries
            { common: E.universalSeriesDefault {name = Just "fst"}
            , lineSeries: E.lineSeriesDefault{"data" = Just $ map simpleData line}
            }
        , E.BarSeries
            { common: E.universalSeriesDefault{name = Just "snd"}
            , barSeries: E.barSeriesDefault{"data" = Just $ map simpleData bar}
            }
        ]
    }


options ∷ ∀ e. Eff (random ∷ RANDOM|e) E.Option
options = do
  line ← lineData
  bar ← barData
  pure $ options_ line bar

subscribe ∷ ∀ e. E.EChart → Eff (echarts ∷ E.ECHARTS|e) E.Sub
subscribe chart = do
  let sub = \et hndl → E.listen et hndl chart
  sub E.ClickEvent DT.traceAnyA
  sub E.DoubleClickEvent DT.traceAnyA
  sub E.DataZoomEvent DT.traceAnyA
  sub E.LegendSelectedEvent DT.traceAnyA
  sub E.MagicTypeChangedEvent DT.traceAnyA
  sub E.DataViewChangedEvent DT.traceAnyA

events
  ∷ ∀ e
  . ElementId
  → Eff (dom ∷ DOM, console ∷ CONSOLE, random ∷ RANDOM, echarts ∷ E.ECHARTS|e) Unit
events id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in events"
    Just el → do
      opts ← options
      chart ←
        E.init Nothing el
          >>= E.setOption opts true
      subscribe chart
      pure unit
