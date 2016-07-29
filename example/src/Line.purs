module Line where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Now (NOW, now)

import Data.Array (head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Formatter.DateTime as FDT
import Data.DateTime as D
import Data.DateTime.Instant (Instant, toDateTime, unInstant, instant)
import Data.Time.Duration (Milliseconds(..))

import DOM (DOM)
import DOM.Node.Types (ElementId(..))

import Debug.Trace as DT

import ECharts.Chart as EC
import ECharts.Types as ET
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types.Phantom (I)
import ECharts.Types.Phantom as ETP

import Signal (Signal, runSignal, (~>), foldp)
import Signal.Time (every)

import Utils as U

startOptions ∷ DSL ETP.OptionI
startOptions = do
  E.title do
    E.text "Dynamic line"
  E.tooltip do
    E.trigger ET.AxisTrigger
    E.formatterAxis \params → head params # maybe ("Incorrect date") _.name
    E.animationEnabled false
  E.xAxis do
    E.splitLine E.hidden
    E.axisType ET.Time

  E.yAxis do
    E.axisType ET.Value
    E.splitLine E.hidden
    E.boundaryGap $ ET.Point { x: ET.Pixel 0, y: ET.Percent 100.0 }

  E.series $ E.line do
    E.name "Dynamic line"
    E.hoverAnimationEnabled false
    E.showSymbol false
  pure unit

type Accum =
  { dt ∷ D.DateTime
  , value ∷ Number
  , values ∷ Array (DSL ETP.ItemI)
  }

dataStream
  ∷ ∀ e i. Accum → Signal (Eff (random ∷ RANDOM|e) (DSL (items ∷ I|i)))
dataStream start =
  accumStream ~> map (E.itemsDSL <<< _.values)
  where
  accumStream =
    foldp foldFn (pure start) (every 2000.0)

  foldFn _ eAcc = do
    acc ← eAcc
    ran ← random
    let
      oneDay = Milliseconds (24.0 * 3600.0 * 1000.0)
      newTime = fromMaybe acc.dt $ D.adjust oneDay acc.dt
      newTimeLabel = either (const $ "Incorrect date") id $ FDT.formatDateTime "YYYY-MM-DD" newTime
      newValue = acc.value + (ran * 21.0 - 10.0)

      newItem ∷ DSL ETP.ItemI
      newItem = do
        E.name newTimeLabel
        E.valuePair newTimeLabel newValue

    pure { value: newValue, dt: newTime, values: [newItem] <> acc.values }

optStream ∷ ∀ e. Accum → Signal (Eff (now ∷ NOW, random ∷ RANDOM|e) (DSL ETP.OptionI))
optStream acc =
  dataStream acc ~> \effItemsSet → do
    itemsSet ← effItemsSet
    pure $ E.series $ E.line itemsSet

chart ∷ ∀ e. Eff (now ∷ NOW, dom ∷ DOM, err ∷ EXCEPTION, echarts ∷ ET.ECHARTS, random ∷ RANDOM|e) Unit
chart = do
  mbEl ← U.getElementById $ ElementId "line"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with line id"
    Just el → do
      ch ← EC.init el
      EC.setOption startOptions ch
      timeStart ← map toDateTime now
      valueStart ← random
      runSignal $ optStream {dt: timeStart, value: valueStart, values: []} ~> \effOpt → do
        opt ← effOpt
        EC.setOption opt ch
