module Main where

import Prelude

import Effect (Effect)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
-- import Control.Monad.Eff.Random (RANDOM)
-- import Control.Monad.Eff.Now (NOW)

-- import DOM (DOM)

-- import ECharts.Types as ET

import Line as Line
import Scatter as Scatter
import Pie as Pie
import Bar as Bar
import Gauge as Gauge
import Funnel as Funnel
import Radar as Radar
import Graph as Graph
import K as K
import Heatmap as Heatmap
import HeatmapCalendar as HeatmapCalendar

import Utils as U

main ∷ Effect Unit
main = U.onLoad do
  Line.chart
  Scatter.chart
  Pie.chart
  Bar.chart
  Gauge.chart
  Funnel.chart
  Radar.chart
  Graph.chart
  K.chart
  Heatmap.chart
  HeatmapCalendar.chart
