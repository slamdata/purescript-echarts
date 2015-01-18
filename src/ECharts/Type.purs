module ECharts.Type where

import Data.Argonaut.Core
import Data.Argonaut.Encode

data ChartType = Line | Bar | Scatter | Candlestick | Pie | Radar
               | Chord | Force | Map | Gauge | Funnel | EventRiver


instance chartTypeShow :: Show ChartType where
  show ct = case ct of
    Line -> "line"
    Bar -> "bar"
    Scatter -> "scatter"
    Candlestick -> "candlestick"
    Pie -> "pie"
    Radar -> "radar"
    Chord -> "chord"
    Force -> "force"
    Map -> "map"
    Gauge -> "gauge"
    Funnel -> "funnel"
    EventRiver -> "eventRiver"

instance chartTypeEncodeJson :: EncodeJson ChartType where
  encodeJson = encodeJson <<< show

