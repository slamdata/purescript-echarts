module Scatter where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array (zipWith)
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..), uncurry)

import Debug.Trace as DT

import DOM (DOM)
import DOM.Node.Types (ElementId(..))

import ECharts.Chart as EC
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL)

import Math (cos, sin, (%))

import Utils as U

genSinData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array ET.Item)
genSinData = do
  randomIs ← map NE.oneOf $ U.randomArray 10000
  randomXs ← map NE.oneOf $ U.randomArray 10000

  let
    randoms =
      zipWith (\i x → Tuple (U.precise 3.0 $ i * 10.0) x)  randomIs randomXs
    mapfn (Tuple i rnd) =
      uncurry ET.pairItem
      $ Tuple i (U.precise 3.0 $ sin i - i * (if i `mod` 2.0 > 0.0 then 0.1 else -0.1) * rnd)
  pure $ map mapfn randoms

genCosData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array ET.Item)
genCosData = do
  randomIs ← map NE.oneOf $ U.randomArray 10000
  randomXs ← map NE.oneOf $ U.randomArray 10000
  let
    randoms =
      zipWith (\i x → Tuple (U.precise 3.0 $ i * 10.0) x)  randomIs randomXs
    mapfn (Tuple i rnd) =
      uncurry ET.pairItem
      $ Tuple i (U.precise 3.0 $ cos i - i * (if i % 2.0 > 0.0 then 0.1 else -0.1) * rnd)
  pure $ map mapfn randoms

options ∷ Array ET.Item → Array ET.Item → DSL ETP.OptionI
options sinData cosData = do
  E.title do
    E.text "SIN and COS random scatter"

  E.tooltip do
    E.trigger ET.AxisTrigger
    E.showDelay 0.0
    E.axisPointer do
      E.shown
      E.pointerType ET.CrossPointer
      E.lineStyle do
        E.lineType ET.DashedLine
        E.width 1
    E.zlevel 1

  E.legend do
    E.items $ map ET.strItem [ "sin", "cos" ]

  E.xAxis do
    E.axisType ET.Value
    E.scale true

  E.yAxis do
    E.axisType ET.Value
    E.scale true

  E.series do
    E.scatter do
      E.name "sin"
      E.large true
      E.symbolSize 3
      E.items sinData

    E.scatter do
      E.name "cos"
      E.large true
      E.symbolSize 2
      E.items cosData

chart ∷ ∀ e. Maybe EC.Theme → Eff (dom ∷ DOM, echarts ∷ ET.ECHARTS, err ∷ EXCEPTION, random ∷ RANDOM|e) Unit
chart theme = do
  mbEl ← U.getElementById $ ElementId "scatter"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with scatter id"
    Just el → do
      ch ← EC.init el theme
      sinData ← genSinData
      cosData ← genCosData
      EC.setOption (options sinData cosData) ch
