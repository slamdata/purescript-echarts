module Radar3 where
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
  Indicator $ indicatorDefault{"text" = Just text, "max" = Just max}

datPair val name =
  Dat $ (dataDefault $ Many val) {name = Just name}

option = Option $ optionDefault {
  "polar" = Just $ [Polar polarDefault {
     "indicator" = Just [
        indicator "sales" 6000,
        indicator "Administration" 16000,
        indicator "IT" 30000,
        indicator "Development" 52000,
        indicator "Customer Support" 38000,
        indicator "Marketing" 25000
        ]
     }],
  "series" = Just $ Just <$> [
    RadarSeries {
       "common": universalSeriesDefault{"name" = Just "budget vs spending"},
       "special": radarSeriesDefault{
         "data" = Just [
            datPair [4300, 10000, 28000, 35000, 50000, 19000] "Allocated",
            datPair [5000, 14000, 28000, 31000, 42000, 21000] "Actual"
            ]
         }
       }
    ]
  }

radar3 id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOption option true

  return unit
