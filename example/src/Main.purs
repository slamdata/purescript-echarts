module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(Just))

import DOM (DOM)

import ECharts.Chart as EC
import ECharts.Types as ET

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
import Themes as Themes

import Utils as U

main ∷ ∀ e. Eff (now ∷ NOW, dom ∷ DOM, echarts ∷ ET.ECHARTS, err ∷ EXCEPTION, random ∷ RANDOM|e) Unit
main = U.onLoad do
  EC.registerTheme "dark" Themes.themeDark
  let theme = Just $ EC.ByName "dark"
  Line.chart theme
  Scatter.chart theme
  Pie.chart theme
  Bar.chart theme
  Gauge.chart theme
  Funnel.chart theme
  Radar.chart theme
  Graph.chart theme
  K.chart theme
  Heatmap.chart theme
