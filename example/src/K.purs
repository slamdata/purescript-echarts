module K where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foldable as F
import Data.Maybe (Maybe(..))

import Debug.Trace as DT

import DOM (DOM)
import DOM.Node.Types (ElementId(..))

import ECharts.Chart as EC
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL)

import Utils as U

options ∷ DSL ETP.OptionI
options = do
  E.xAxis do
    E.axisType ET.Category
    E.items $ map ET.strItem ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30" ]
  E.yAxis do
    E.axisType ET.Value
    E.min 2200.0
    E.scale true
  E.series $ E.candlestick do
    E.buildItems
      $ F.traverse_ (E.addItem <<< E.values)
      [ [ 2320.26, 2302.6, 2287.3, 2362.94 ]
      , [ 2300.00, 2291.3, 2288.26, 2308.38 ]
      , [ 2295.35, 2346.5, 2295.35, 2346.92 ]
      , [ 2347.22, 2358.98, 2337.35, 2363.8 ]
      , [ 2360.75, 2382.48, 2347.89, 2383.76 ]
      ]

chart ∷ ∀ e. Eff (dom ∷ DOM, echarts ∷ ET.ECHARTS, err ∷ EXCEPTION|e) Unit
chart = do
  mbEl ← U.getElementById $ ElementId "k"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with 'k' id"
    Just el → do
      ch ← EC.init el Nothing
      EC.setOption options ch
