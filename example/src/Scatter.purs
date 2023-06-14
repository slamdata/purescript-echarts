module Scatter where

import Prelude

import Effect (Effect)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Eff.Random (RANDOM)
import Data.Array (zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty as NE
import Data.Tuple (Tuple(..), uncurry)
-- import Debug.Trace as DT
-- import DOM (DOM)
import DOM.Node.Types (ElementId)
import ECharts.Chart as EC
import ECharts.Theme as ETheme
import ECharts.Types as ET
-- import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)
import Data.Number (cos, sin, (%))
import Utils as U

genSinData ∷ Effect (Array ET.Item)
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

genCosData ∷ Effect (Array ET.Item)
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

options ∷ Array ET.Item → Array ET.Item → DSL' -- ETP.OptionI
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
      E.markPoint do
        E.label $ E.normal $ E.shown
        E.buildItems $ E.addItem do
          E.buildCoord do
            E.coordXValue "6.0"
            E.coordY "1.3"
          E.symbolSize 30
          E.symbol ET.Rect

      E.name "sin"
      E.large true
      E.symbolSize 3
      E.items sinData

    E.scatter do
      E.name "cos"
      E.large true
      E.symbolSize 2
      E.items cosData

  E.dataZoom do
    E.sliderDataZoom E.shown
    E.insideDataZoom $ pure unit

chart ∷ Effect Unit
chart = do
  chart' ("scatter-1") Nothing
  chart' ("scatter-2") (Just (ETheme.dark))
  chart' ("scatter-3") (Just (ETheme.macarons))

chart' ∷ ElementId → Maybe ETheme.Theme → Effect Unit
chart' id theme = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → pure unit -- DT.traceAnyA "There is no element with scatter id"
    Just el → do
      ch ← maybe EC.init EC.initWithTheme theme el
      sinData ← genSinData
      cosData ← genCosData
      EC.setOption (interpret $ options sinData cosData) ch
