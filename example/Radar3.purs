module Radar3 where

import Prelude
import Control.Monad.Eff.Console (print)
import Data.Tuple.Nested
import Data.Array hiding (init)

import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Axis
import ECharts.Item.Data
import ECharts.Item.Value
import Data.Maybe
import Utils

indicator text max =
  Indicator $ indicatorDefault{text = Just text, max = Just max}

datPair val name =
  Dat $ (dataDefault $ Many val) {name = Just name}

option = Option $ optionDefault {
  polar = Just $ [Polar polarDefault {
     indicator = Just [
        indicator "sales" 6000.0,
        indicator "Administration" 16000.0,
        indicator "IT" 30000.0,
        indicator "Development" 52000.0,
        indicator "Customer Support" 38000.0,
        indicator "Marketing" 25000.0
        ]
     }],
  series = Just $ Just <$> [
    RadarSeries {
       common: universalSeriesDefault{name = Just "budget vs spending"},
       radarSeries: radarSeriesDefault{
         "data" = Just [
            datPair [4300.0, 10000.0, 28000.0, 35000.0, 50000.0, 19000.0] "Allocated",
            datPair [5000.0, 14000.0, 28000.0, 31000.0, 42000.0, 21000.0] "Actual"
            ]
         }
       }
    ]
  }

radar3 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in radar3"
    Just el -> init Nothing el >>= setOption option true >>= \_ -> return unit

