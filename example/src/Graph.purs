module Graph where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

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
  E.title $ E.text "Graph"
  E.tooltip $ pure unit
  E.animationDurationUpdate 1500
  E.animationEasingUpdateQuinticInOut
  E.series do
    E.graph do
      E.layoutNone
      E.symbolSize 50
      E.roam true
      E.label $ E.normalLabel E.shown

      E.edgeSymbols do
        E.circleEdgeSymbol
        E.arrowEdgeSymbol
      E.edgeSymbolSizes 4 10

      E.edgeLabel $ E.normalEdgeLabel $ E.textStyle $ E.fontSize 20

      E.lineStylePair$ E.normalLineStyle do
        E.opacity 0.9
        E.width 2
        E.curveness 0.0

      E.buildItems do
        E.addItem do
          E.name "one"
          E.x 300.0
          E.y 300.0
        E.addItem do
          E.name "two"
          E.x 600.0
          E.y 300.0
        E.addItem do
          E.name "three"
          E.x 450.0
          E.y 100.0
        E.addItem do
          E.name "four"
          E.x 450.0
          E.y 500.0
      E.buildLinks do
        E.addLink do
          E.sourceIx 0
          E.targetIx 1
          E.symbolSizes 5 20
          E.label $ E.normalLabel E.shown
          E.lineStylePair $ E.normalLineStyle do
            E.width 5
            E.curveness 0.2
        E.addLink do
          E.sourceName "two"
          E.targetName "three"
          E.label $ E.normalLabel E.shown
          E.lineStylePair$ E.normalLineStyle $ E.curveness 0.2
        E.addLink do
          E.sourceName "one"
          E.targetName "three"
        E.addLink do
          E.sourceName "two"
          E.targetName "three"
        E.addLink do
          E.sourceName "two"
          E.targetName "four"
        E.addLink do
          E.sourceName "one"
          E.targetName "four"

chart ∷ ∀ e. Maybe EC.Theme → Eff (dom ∷ DOM, echarts ∷ ET.ECHARTS, err ∷ EXCEPTION|e) Unit
chart theme = do
  mbEl ← U.getElementById $ ElementId "graph"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with 'graph' id"
    Just el → do
      ch ← EC.init el theme
      EC.setOption options ch
