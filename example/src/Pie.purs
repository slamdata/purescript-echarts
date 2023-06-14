module Pie where

import Prelude

import Effect (Effect)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))
--- import Data.Symbol (SProxy(..))
import Type.Proxy (Proxy (..))
import Data.Variant as V
-- import Debug.Trace as DT
-- import DOM (DOM)
-- import DOM.Node.Types (ElementId)
import ECharts.Chart as EC
import ECharts.Event as EE
import ECharts.Types as ET
-- import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)
import Utils as U

options ∷ DSL' -- ETP.OptionI
options = do
  E.tooltip do
    E.trigger ET.ItemTrigger
    E.formatterString "{a} <br /> {b}: {c} ({d}%)"
  E.legend do
    E.orient ET.Vertical
    E.leftPosition $ ET.LeftHP
    E.items
      $ map ET.strItem
        [ "one", "two", "three", "four", "five", "six", "seven", "eight" ]
  E.series do
    E.pie do
      E.name "Inner"
      E.selectedMode ET.Single
      E.radius $ ET.Radius { start: ET.Pixel 0, end: ET.Percent 30.0 }
      E.label $ E.normalLabel E.hidden
      E.buildItems do
        E.addItem do
          E.value 335.0
          E.name "one"
          E.selected true
        E.addItem do
          E.value 679.0
          E.name "two"
        E.addItem do
          E.value 1548.0
          E.name "three"
    E.pie do
      E.name "Outer"
      E.selectedMode ET.Multiple
      E.radius $ ET.Radius { start: ET.Percent 40.0, end: ET.Percent 55.0 }
      E.buildItems do
        E.addItem do
          E.value 335.0
          E.name "one"
        E.addItem do
          E.value 310.0
          E.name "two"
        E.addItem do
          E.value 234.0
          E.name "three"
        E.addItem do
          E.value 135.0
          E.name "four"
        E.addItem do
          E.value 1048.0
          E.name "five"
        E.addItem do
          E.value 251.0
          E.name "six"
        E.addItem do
          E.value 147.0
          E.name "seven"
        E.addItem do
          E.value 102.0
          E.name "eight"


chart ∷ Effect Unit
chart = do
  mbEl ← U.getElementById "pie"
  case mbEl of
    Nothing → pure unit -- DT.traceAnyA "There is no element with pie id"
    Just el → do
      ch ← EC.init el
      EC.setOption (interpret options) ch
      EE.listenAll ch (\_ -> pure unit) --- DT.traceAnyA
      EE.dispatch
        (V.inj (Proxy ∷ Proxy "pieselected")
           $  { seriesIndex: 1
              , seriesName: "Outer"
              , name: "seven"
              , dataIndex: 4
              })
        ch
