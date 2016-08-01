module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Now (NOW)

import DOM (DOM)

import ECharts.Types as ET

import Line as Line
import Scatter as Scatter
import Pie as Pie
import Bar as Bar
import Gauge as Gauge
import Funnel as Funnel
import Radar as Radar

import Utils as U

main ∷ ∀ e. Eff (now ∷ NOW, dom ∷ DOM, echarts ∷ ET.ECHARTS, err ∷ EXCEPTION, random ∷ RANDOM|e) Unit
main = U.onLoad do
  Line.chart
  Scatter.chart
  Pie.chart
  Bar.chart
  Gauge.chart
  Funnel.chart
  Radar.chart
