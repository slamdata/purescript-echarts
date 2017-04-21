module Gauge where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, random)

import Data.Maybe (Maybe(..))

import Debug.Trace as DT

import DOM (DOM)
import DOM.Node.Types (ElementId(..))

import ECharts.Chart as EC
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL)

import Signal (Signal, runSignal, (~>))
import Signal.Time (every)

import Utils as U

options ∷ {speed ∷ Number, r ∷ Number, gas ∷ Number, water ∷ Number} → DSL ETP.OptionI
options obj = do
  E.tooltip $ E.formatterString "{a} <br />{c} {b}"

  E.toolbox do
    E.shown
    E.feature do
      E.restore E.shown
      E.saveAsImage E.shown

  E.series do
    E.gauge do
      E.name "SPEED"
      E.z 3
      E.min 0.0
      E.max 220.0
      E.splitNumber 11
      E.axisLine do
        E.lineStyle $ E.width 10
      E.axisTick do
        E.length 15
        E.lineStyle E.autoColor
      E.splitLine do
        E.length 20
        E.lineStyle E.autoColor
      E.title $ E.textStyle do
        E.bolderFontWeight
        E.fontSize 20
        E.italicFontStyle

      E.detail $ E.textStyle do
        E.bolderFontWeight

      E.buildItems $ E.addItem do
        E.name "km/h"
        E.value obj.speed

    E.gauge do
      E.name "R/MIN"
      E.center $ ET.Point { x: ET.Percent 20.0, y: ET.Percent 55.0 }
      E.gaugeRadius $ ET.Percent 35.0
      E.min 0.0
      E.max 7.0
      E.endAngle 45.0
      E.splitNumber 7
      E.axisLine $ E.lineStyle $ E.width 8
      E.axisTick do
        E.length 12
        E.lineStyle E.autoColor
      E.splitLine do
        E.length 20
        E.lineStyle E.autoColor
      E.gaugePointer $ E.width 5
      E.title $ E.offsetCenter $ ET.Point { x: ET.Pixel 0, y: ET.Percent (-30.0) }
      E.detail $ E.textStyle E.bolderFontWeight
      E.buildItems $ E.addItem do
        E.value obj.r
        E.name "x1000 r/min"

    E.gauge do
      E.name "GAS"
      E.center $ ET.Point { x: ET.Percent 77.0, y: ET.Percent 50.0 }
      E.gaugeRadius $ ET.Percent 25.0
      E.min 0.0
      E.max 2.0
      E.startAngle 135.0
      E.endAngle 45.0
      E.splitNumber 2
      E.axisLine $ E.lineStyle $ E.width 8
      E.axisTick do
        E.splitNumber 5
        E.length 10
        E.lineStyle E.autoColor
      E.axisLabel $ E.formatterValue \inp →
        case inp of
          0.0 → "E"
          1.0 → "Gas"
          2.0 → "F"
          _ → "Err"
      E.splitLine do
        E.length 15
        E.lineStyle E.autoColor
      E.gaugePointer $ E.width 2
      E.title E.hidden
      E.detail E.hidden
      E.buildItems $ E.addItem do
        E.name "gas"
        E.value obj.gas

    E.gauge do
      E.name "WATER"
      E.center $ ET.Point { x: ET.Percent 77.0, y: ET.Percent 50.0 }
      E.gaugeRadius $ ET.Percent 25.0
      E.min 0.0
      E.max 2.0
      E.startAngle 315.0
      E.endAngle 225.0
      E.splitNumber 2
      E.axisLine $ E.lineStyle $ E.width 8
      E.axisTick E.hidden
      E.axisLabel $ E.formatterValue \inp →
        case inp of
          0.0 → "H"
          1.0 → "Water"
          2.0 → "C"
          _ → "Err"
      E.splitLine do
        E.length 15
        E.lineStyle E.autoColor
      E.gaugePointer $ E.width 8
      E.title E.hidden
      E.detail E.hidden
      E.buildItems $ E.addItem do
        E.name "water"
        E.value obj.water


initialVal ∷ {speed ∷ Number, r ∷ Number, gas ∷ Number, water ∷ Number}
initialVal =
  { speed: 40.0
  , r: 1.5
  , gas: 0.5
  , water: 0.5
  }



dataStream
  ∷ ∀ e. Signal (Eff (random ∷ RANDOM|e) {speed ∷ Number, r ∷ Number, gas ∷ Number, water ∷ Number })
dataStream =
  every 2000.0 ~> \_ → do
    speed ← random <#> (mul 100.0 >>> U.precise 2.0)
    r ← random <#> (mul 7.0 >>> U.precise 2.0)
    gas ← random <#> (mul 2.0 >>> U.precise 2.0)
    water ← random <#> (mul 2.0 >>> U.precise 2.0)
    pure {speed, r, gas, water}



chart ∷ ∀ e. Eff (random ∷ RANDOM, dom ∷ DOM, echarts ∷ ET.ECHARTS, exception ∷ EXCEPTION|e) Unit
chart = do
  mbEl ← U.getElementById $ ElementId "gauge"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with 'gauge' id"
    Just el → do
      ch ← EC.init el
      EC.setOption (options initialVal) ch
      runSignal $ dataStream ~> \effVal → do
        val ← effVal
        EC.setOption (options val) ch
