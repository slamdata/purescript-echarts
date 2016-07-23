module DynamicLineBar where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.Eff.Now (now, NOW)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((..))
import Data.Either (either)
import Data.Traversable (sequence_, sequence)
import Data.Int (toNumber)
import Data.Formatter.DateTime as FDT
import Data.DateTime.Instant (toDateTime, unInstant, instant)
import Data.Time.Duration (Milliseconds(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Math (round)

import Signal (Signal, runSignal, (~>))
import Signal.Time (every)

import Utils as U

xTimeAxis ∷ ∀ e. Eff (now ∷ NOW|e) (Array String)
xTimeAxis = do
  start ← map unInstant now
  let
    mapfn i =
      let
        thisMs = start - Milliseconds (toNumber i * 2000.0)
        thisInstant = fromMaybe bottom $ instant thisMs
        thisDT = toDateTime thisInstant
      in
        FDT.formatDateTime "HH:mm:ss" thisDT
        # either (const "This shouldn't happen") id
  pure $ mapfn <$> (1..10)

data2 ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array Number)
data2 = do
  let
    mapfn i = do
      rnd ← random
      pure $ U.precise 1.0 $  rnd * 10.0 + 5.0
  sequence $ (mapfn <<< toNumber) <$> (1..10)

data1 ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array Number)
data1 = do
  let
    mapfn i = do
      rnd ← random
      pure $ round (rnd * 1000.0)
  sequence $ (toNumber >>> mapfn) <$> (1..10)

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple


options_ ∷ Array String → Array Number → Array Number → E.Option
options_ xAxis d1 d2 =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerAxis}
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault ["new price", "pre-order queue"] }
    , title = Just $ E.Title E.titleDefault
        { text = Just "dynamic data"
        , subtext = Just "fictitious"
        }
    , toolbox = Just $ E.Toolbox $ E.toolboxDefault
        { show = Just true
        , feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature E.markFeatureDefault {show = Just true}
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
    , dataZoom = Just $ E.DataZoom $ E.dataZoomDefault
        { show = Just true
        , start = Just 0.0
        , end = Just 100.0
        }
    , xAxis =
        Just $ E.TwoAxises
          (E.Axis E.axisDefault
             { "type" = Just E.CategoryAxis
             , boundaryGap = Just $ E.CatBoundaryGap true
             , "data" = Just $ map E.CommonAxisData xAxis
             })
          (E.Axis E.axisDefault
             { "type" = Just E.CategoryAxis
             , boundaryGap = Just $ E.CatBoundaryGap true
             , "data" = Just $ map (E.CommonAxisData <<< show) (1..10)
             })
    , yAxis =
        Just $ E.TwoAxises
          (E.Axis E.axisDefault
             { "type" = Just E.ValueAxis
             , scale = Just true
             , boundaryGap = Just $ E.ValueBoundaryGap 0.2 0.2
             , name = Just "price"
             })
          (E.Axis E.axisDefault
             { "type" = Just E.ValueAxis
             , scale = Just true
             , name = Just "pre-order"
             , boundaryGap = Just $ E.ValueBoundaryGap 0.2 0.2
             , nameTextStyle = Just $ E.TextStyle E.textStyleDefault
                 { align = Just E.HAlignLeft }
             })
    , series = Just $ map Just
        [ E.BarSeries
            { common: E.universalSeriesDefault { name = Just "pre-order queue" }
            , barSeries: E.barSeriesDefault
                { xAxisIndex = Just 1.0
                , yAxisIndex = Just 1.0
                , "data" = Just $ map simpleData d1
                }
            }
        , E.LineSeries
            { common: E.universalSeriesDefault { name = Just "new price" }
            , lineSeries: E.lineSeriesDefault { "data" = Just $ map simpleData d2 }
            }
        ]
    }


options ∷ ∀ e. Eff (now ∷ NOW, random ∷ RANDOM|e) E.Option
options = do
  xAxs ← xTimeAxis
  d1 ← data1
  d2 ← data2
  pure $ options_ xAxs d1 d2


dataStream ∷ ∀ e. Signal (Eff (now ∷ NOW, random ∷ RANDOM|e) (Array E.AdditionalData))
dataStream =
  every 2000.0 ~> const do
    rnd1 ← random
    rnd2 ← random
    rnd3 ← random
    curTime ← map toDateTime now
    let lastData =
          U.precise 1.0
          $ rnd1
          * if round ((rnd2 * 10.0) `mod` 2.0) == 0.0 then 1.0 else -1.0

        axisData =
          FDT.formatDateTime "HH:mm:ss" curTime
          # either (const "This shouldn't happen") id
        firstData =
          E.AdditionalData
            { idx: 0.0
            , datum: E.Value $ E.Simple $ round (rnd3 * 1000.0)
            , isHead: true
            , dataGrow: false
            , additionalData: Nothing
            }
        sndData =
          E.AdditionalData
            { idx: 1.0
            , datum: E.Value $ E.Simple $ lastData
            , isHead: false
            , dataGrow: false
            , additionalData: Just axisData
            }
    pure [firstData, sndData]


dynamicLineBar
  ∷ ∀ e
  . ElementId
  → Eff (dom ∷ DOM, now ∷ NOW, random ∷ RANDOM, console ∷ CONSOLE, echarts ∷ E.ECHARTS|e) Unit
dynamicLineBar id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "Inocrrect id in dymaniclinebar"
    Just el → do
      opts ← options
      chart ← E.init Nothing el >>= E.setOption opts true
      runSignal $ dataStream ~> \effContent → do
        content ← effContent
        sequence_ $ (flip E.addData) chart <$> content
