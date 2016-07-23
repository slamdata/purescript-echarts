module Main where

import Prelude


import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)

import DOM (DOM)
import DOM.Node.Types (ElementId(..))

import ECharts (ECHARTS)

import Chord2 (chord2)
import Connect (connectM)
import Control.Monad.Eff (Eff())
import DynamicLineBar (dynamicLineBar)
import EventRiver1 (eventRiver)
import Events (events)
import Force4 (force4)
import Funnel2 (funnel2)
import Gauge4 (gauge4)
import K (k)
import Line4 (line4)
import Loading (loading)
import Map11 (map11)
import Mix2Safe (mix2safe)
import Radar3 (radar3)
import Scatter3 (scatter3)
import Bubble (bubble)
import AreaPlot (areaPlot)
import ConfidenceBand (confidenceBand)
import PercentageArea (percentageArea)
import Utils (onLoad)


main ∷ ∀ e. Eff (dom ∷ DOM, echarts ∷ ECHARTS, random ∷ RANDOM, now ∷ NOW, console ∷ CONSOLE|e) Unit
main = onLoad $ do
  line4 (ElementId "line4")
  scatter3 (ElementId "scatter3")
  bubble (ElementId "bubble")
  k (ElementId "k")
  radar3 (ElementId "radar3")
  chord2 (ElementId "chord2")
  force4 (ElementId "force4")
  map11 (ElementId "map11")
  gauge4 (ElementId "gauge4")
  funnel2 (ElementId "funnel2")
  eventRiver (ElementId "event-river")

  dynamicLineBar (ElementId "dynamic-line-bar")
  loading (ElementId "loading")
  events (ElementId "events")
  connectM (ElementId "connect1") (ElementId "connect2")
  mix2safe (ElementId "mix2safe")
  areaPlot (ElementId "area-plot")
  confidenceBand (ElementId "confidence-band")
  percentageArea (ElementId "percentage-area")
