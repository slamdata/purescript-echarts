module Radar where

import Prelude

import Color as C

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
import ECharts.Monad (DSL', interpret)

import Utils as U

lineStyle ∷ DSL' ETP.LineStylePairI
lineStyle = E.normalLineStyle do
  E.width 1
  E.opacity 0.5


options ∷ DSL' ETP.OptionI
options = do
  F.for_ (C.fromHexString "#161627") E.backgroundColor

  E.title do
    E.text "AQI - Radar"
    E.leftCenter

    E.textStyle do
      F.for_ (C.fromHexString "#eee") E.color

  E.legend do
    E.bottom $ ET.Pixel 5
    E.items $ map ET.strItem ["Beijing", "Shanghai", "Guanzhou"]
    E.itemGap 20
    E.textStyle do
      F.for_ (C.fromHexString "#fff") E.color
      E.fontSize 14
    E.selectedMode ET.Single


  E.radar do
    E.indicators do
      E.indicator do
        E.name "AQI"
        E.max 300.0
      E.indicator do
        E.name "PM2.5"
        E.max 250.0
      E.indicator do
        E.name "PM10"
        E.max 300.0
      E.indicator do
        E.name "CO"
        E.max 5.0
      E.indicator do
        E.name "NO2"
        E.max 200.0
      E.indicator do
        E.name "SO2"
        E.max 100.0
    E.circleShape
    E.splitNumber 5
    E.radarName do
      E.textStyle do
        E.color $ C.rgba 238 197 102 1.0
    E.splitLine do
      E.lineStyle do
        E.rgbaColors
          [ C.rgba 238 197 102 1.0
          , C.rgba 238 197 102 0.8
          , C.rgba 238 197 102 0.6
          , C.rgba 238 197 102 0.4
          , C.rgba 238 197 102 0.2
          , C.rgba 238 197 102 0.1
          ]
    E.splitArea E.hidden
    E.axisLine do
      E.lineStyle $ E.rgbaColor $ C.rgba 238 197 102 0.5

  E.series do
    E.radarSeries do
      E.name "Beijing"
      E.lineStylePair lineStyle
      E.symbol ET.None
      E.itemStyle do
        E.normalItemStyle do
          F.for_ (C.fromHexString "#f9713c") E.color
      E.areaStylePair $ E.normalAreaStyle $ E.opacity 0.1
      E.buildItems do
        F.for_ dataBJ (E.addItem <<< E.values)
    E.radarSeries do
      E.name "Shanghai"
      E.lineStylePair lineStyle
      E.symbol ET.None
      E.itemStyle $ E.normalItemStyle $ F.for_ (C.fromHexString "#B3E4A1") E.color
      E.areaStylePair $ E.normalAreaStyle $ E.opacity 0.05
      E.buildItems do
        F.for_ dataSH (E.addItem <<< E.values)
    E.radarSeries do
      E.name "Guanzhou"
      E.lineStylePair lineStyle
      E.symbol ET.None
      E.itemStyle $ E.normalItemStyle $ E.rgbaColor $ C.rgba 238 197 102 1.0
      E.areaStylePair $ E.normalAreaStyle $ E.opacity 0.05
      E.buildItems do
        F.for_ dataGZ (E.addItem <<< E.values)

chart ∷ ∀ e. Eff (dom ∷ DOM, echarts ∷ ET.ECHARTS, exception ∷ EXCEPTION|e) Unit
chart = do
  mbEl ← U.getElementById $ ElementId "radar"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with 'radar' id"
    Just el → do
      ch ← EC.init el
      EC.setOption (interpret options) ch

dataBJ ∷ Array (Array Number)
dataBJ =
  [ [55.0, 9.0, 56.0, 0.46, 18.0, 6.0, 1.0]
  , [25.0, 11.0, 21.0, 0.65, 34.0, 9.0, 2.0]
  , [56.0, 7.0, 63.0, 0.3, 14.0, 5.0, 3.0]
  , [33.0, 7.0, 29.0, 0.33, 16.0, 6.0, 4.0]
  , [42.0, 24.0, 44.0, 0.76, 40.0, 16.0, 5.0]
  , [82.0, 58.0, 90.0, 1.77, 68.0, 33.0, 6.0]
  , [74.0, 49.0, 77.0, 1.46, 48.0, 27.0, 7.0]
  , [78.0, 55.0, 80.0, 1.29, 59.0, 29.0, 8.0]
  , [267.0, 216.0, 280.0, 4.8, 108.0, 64.0, 9.0]
  , [185.0, 127.0, 216.0, 2.52, 61.0, 27.0, 10.0]
  , [39.0, 19.0, 38.0, 0.57, 31.0, 15.0, 11.0]
  , [41.0, 11.0, 40.0, 0.43, 21.0, 7.0, 12.0]
  , [64.0, 38.0, 74.0, 1.04, 46.0, 22.0, 13.0]
  , [108.0, 79.0, 120.0, 1.7, 75.0, 41.0, 14.0]
  , [108.0, 63.0, 116.0, 1.48, 44.0, 26.0, 15.0]
  , [33.0, 6.0, 29.0, 0.34, 13.0, 5.0, 16.0]
  , [94.0, 66.0, 110.0, 1.54, 62.0, 31.0, 17.0]
  , [186.0, 142.0, 192.0, 3.88, 93.0, 79.0, 18.0]
  , [57.0, 31.0, 54.0, 0.96, 32.0, 14.0, 19.0]
  , [22.0, 8.0, 17.0, 0.48, 23.0, 10.0, 20.0]
  , [39.0, 15.0, 36.0, 0.61, 29.0, 13.0, 21.0]
  , [94.0, 69.0, 114.0, 2.08, 73.0, 39.0, 22.0]
  , [99.0, 73.0, 110.0, 2.43, 76.0, 48.0, 23.0]
  , [31.0, 12.0, 30.0, 0.5, 32.0, 16.0, 24.0]
  , [42.0, 27.0, 43.0, 1.0, 53.0, 22.0, 25.0]
  , [154.0, 117.0, 157.0, 3.05, 92.0, 58.0, 26.0]
  , [234.0, 185.0, 230.0, 4.09, 123.0, 69.0, 27.0]
  , [160.0, 120.0, 186.0, 2.77, 91.0, 50.0, 28.0]
  , [134.0, 96.0, 165.0, 2.76, 83.0, 41.0, 29.0]
  , [52.0, 24.0, 60.0, 1.03, 50.0, 21.0, 30.0]
  , [46.0, 5.0, 49.0, 0.28, 10.0, 6.0, 31.0]
  ]

dataGZ ∷ Array (Array Number)
dataGZ =
  [ [26.0, 37.0, 27.0, 1.163, 27.0, 13.0, 1.0]
  , [85.0, 62.0, 71.0, 1.195, 60.0, 8.0, 2.0]
  , [78.0, 38.0, 74.0, 1.363, 37.0, 7.0, 3.0]
  , [21.0, 21.0, 36.0, 0.634, 40.0, 9.0, 4.0]
  , [41.0, 42.0, 46.0, 0.915, 81.0, 13.0, 5.0]
  , [56.0, 52.0, 69.0, 1.067, 92.0, 16.0, 6.0]
  , [64.0, 30.0, 28.0, 0.924, 51.0, 2.0, 7.0]
  , [55.0, 48.0, 74.0, 1.236, 75.0, 26.0, 8.0]
  , [76.0, 85.0, 113.0, 1.237, 114.0, 27.0, 9.0]
  , [91.0, 81.0, 104.0, 1.041, 56.0, 40.0, 10.0]
  , [84.0, 39.0, 60.0, 0.964, 25.0, 11.0, 11.0]
  , [64.0, 51.0, 101.0, 0.862, 58.0, 23.0, 12.0]
  , [70.0, 69.0, 120.0, 1.198, 65.0, 36.0, 13.0]
  , [77.0, 105.0, 178.0, 2.549, 64.0, 16.0, 14.0]
  , [109.0, 68.0, 87.0, 0.996, 74.0, 29.0, 15.0]
  , [73.0, 68.0, 97.0, 0.905, 51.0, 34.0, 16.0]
  , [54.0, 27.0, 47.0, 0.592, 53.0, 12.0, 17.0]
  , [51.0, 61.0, 97.0, 0.811, 65.0, 19.0, 18.0]
  , [91.0, 71.0, 121.0, 1.374, 43.0, 18.0, 19.0]
  , [73.0, 102.0, 182.0, 2.787, 44.0, 19.0, 20.0]
  , [73.0, 50.0, 76.0, 0.717, 31.0, 20.0, 21.0]
  , [84.0, 94.0, 140.0, 2.238, 68.0, 18.0, 22.0]
  , [93.0, 77.0, 104.0, 1.165, 53.0, 7.0, 23.0]
  , [99.0, 130.0, 227.0, 3.97, 55.0, 15.0, 24.0]
  , [146.0, 84.0, 139.0, 1.094, 40.0, 17.0, 25.0]
  , [113.0, 108.0, 137.0, 1.481, 48.0, 15.0, 26.0]
  , [81.0, 48.0, 62.0, 1.619, 26.0, 3.0, 27.0]
  , [56.0, 48.0, 68.0, 1.336, 37.0, 9.0, 28.0]
  , [82.0, 92.0, 174.0, 3.29, 0.0, 13.0, 29.0]
  , [106.0, 116.0, 188.0, 3.628, 101.0, 16.0, 30.0]
  , [118.0, 50.0, 0.0, 1.383, 76.0, 11.0, 31.0]
  ]

dataSH ∷ Array (Array Number)
dataSH =
  [ [91.0, 45.0, 125.0, 0.82, 34.0, 23.0, 1.0]
  , [65.0, 27.0, 78.0, 0.86, 45.0, 29.0, 2.0]
  , [83.0, 60.0, 84.0, 1.09, 73.0, 27.0, 3.0]
  , [109.0, 81.0, 121.0, 1.28, 68.0, 51.0, 4.0]
  , [106.0, 77.0, 114.0, 1.07, 55.0, 51.0, 5.0]
  , [109.0, 81.0, 121.0, 1.28, 68.0, 51.0, 6.0]
  , [106.0, 77.0, 114.0, 1.07, 55.0, 51.0, 7.0]
  , [89.0, 65.0, 78.0, 0.86, 51.0, 26.0, 8.0]
  , [53.0, 33.0, 47.0, 0.64, 50.0, 17.0, 9.0]
  , [80.0, 55.0, 80.0, 1.01, 75.0, 24.0, 10.0]
  , [117.0, 81.0, 124.0, 1.03, 45.0, 24.0, 11.0]
  , [99.0, 71.0, 142.0, 1.1, 62.0, 42.0, 12.0]
  , [95.0, 69.0, 130.0, 1.28, 74.0, 50.0, 13.0]
  , [116.0, 87.0, 131.0, 1.47, 84.0, 40.0, 14.0]
  , [108.0, 80.0, 121.0, 1.3, 85.0, 37.0, 15.0]
  , [134.0, 83.0, 167.0, 1.16, 57.0, 43.0, 16.0]
  , [79.0, 43.0, 107.0, 1.05, 59.0, 37.0, 17.0]
  , [71.0, 46.0, 89.0, 0.86, 64.0, 25.0, 18.0]
  , [97.0, 71.0, 113.0, 1.17, 88.0, 31.0, 19.0]
  , [84.0, 57.0, 91.0, 0.85, 55.0, 31.0, 20.0]
  , [87.0, 63.0, 101.0, 0.9, 56.0, 41.0, 21.0]
  , [104.0, 77.0, 119.0, 1.09, 73.0, 48.0, 22.0]
  , [87.0, 62.0, 100.0, 1.72, 28.0, 23.0]
  , [168.0, 128.0, 172.0, 1.49, 97.0, 56.0, 24.0]
  , [65.0, 45.0, 51.0, 0.74, 39.0, 17.0, 25.0]
  , [39.0, 24.0, 38.0, 0.61, 47.0, 17.0, 26.0]
  , [39.0, 24.0, 39.0, 0.59, 50.0, 19.0, 27.0]
  , [93.0, 68.0, 96.0, 1.05, 79.0, 29.0, 28.0]
  , [188.0, 143.0, 197.0, 1.66, 99.0, 51.0, 29.0]
  , [174.0, 131.0, 174.0, 1.55, 108.0, 50.0, 30.0]
  , [187.0, 143.0, 201.0, 1.39, 89.0, 53.0, 31.0]
  ]
