module K where

import Prelude

--  import Control.Monad.Eff (Eff)
--- import Control.Monad.Eff.Exception (EXCEPTION)

import Effect (Effect)

import Data.Foldable as F
import Data.Maybe (Maybe(..))

--- import Debug.Trace as DT

--- import DOM (DOM)
--- import DOM.Node.Types (ElementId)

import ECharts.Chart as EC
import ECharts.Types as ET
--- import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)

import Utils as U

options ∷ DSL' -- ETP.OptionI
options = do
  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30" ]
  E.yAxis do
    E.axisType ET.Value
    E.min 2200.0
    E.scale true
  E.series $ E.candlestick do
    E.markLine do
      E.lineStyle $ E.normal do
        E.solidLine
      E.buildMarkItems do
        E.addItem do
          E.symbol ET.Diamond
          E.buildCoord do
            E.coordXValue "2013/1/24"
            E.coordY "2200.00"
        E.addItem do
          E.symbol ET.Diamond
          E.buildCoord do
            E.coordXValue "2013/1/29"
            E.coordY "2300.00"

    E.buildItems
      $ F.traverse_ (E.addItem <<< E.values)
      [ [ 2320.26, 2302.6, 2287.3, 2362.94 ]
      , [ 2300.00, 2291.3, 2288.26, 2308.38 ]
      , [ 2295.35, 2346.5, 2295.35, 2346.92 ]
      , [ 2347.22, 2358.98, 2337.35, 2363.8 ]
      , [ 2360.75, 2382.48, 2347.89, 2383.76 ]
      ]

chart ∷ Effect Unit
chart = do
  mbEl ← U.getElementById "k"
  case mbEl of
    Nothing → pure unit --- DT.traceAnyA "There is no element with 'k' id"
    Just el → do
      ch ← EC.init el
      EC.setOption (interpret options) ch
