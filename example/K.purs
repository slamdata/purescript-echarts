module K where

import Debug.Trace (trace)
import Data.Array hiding (init)
import Data.Tuple
import Data.Tuple.Nested
import Data.Maybe
import Utils

import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Axis
import ECharts.Item.Value
import ECharts.Item.Data


simpleDat o c l h = Value $ HLOC {
  h: h, l: l, o: o, c: c
  }

options = Option $ optionDefault {
  "xAxis" = Just $ OneAxis $ Axis axisDefault {
     "type" = Just CategoryAxis,
     "data" = Just $ CommonAxisData <$>
              ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30"]
     },
  "yAxis" = Just $ OneAxis $ Axis axisDefault {
    "type" = Just ValueAxis
    },
  "series" = Just $ Just <$> [
    CandlestickSeries {
       common: universalSeriesDefault,
       candlestickSeries: candlestickSeriesDefault{
         "data" = Just $ [
            simpleDat 2320.26 2302.6 2287.3 2362.94,
            simpleDat 2300 2291.3 2288.26 2308.38,
            simpleDat 2295.35 2346.5 2295.35 2346.92,
            simpleDat 2347.22 2358.98 2337.35 2363.8,
            simpleDat 2360.75 2382.48 2347.89 2383.76
            ]
         }
       }
    ]
  }


k id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> trace "incorrect id in k"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit
