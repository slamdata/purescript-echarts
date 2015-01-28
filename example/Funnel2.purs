module Funnel2 where
import Data.Tuple.Nested
import Data.Maybe
import Utils

import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common

simpleDat val nam =
  Dat $ (dataDefault $ Simple val) {name = Just nam}

options = Option $ optionDefault {
  series = Just $ Just <$> [
     FunnelSeries {
        common: universalSeriesDefault,
        special: funnelSeriesDefault {
          "data" = Just $  [
             simpleDat 60 "foo",
             simpleDat 80 "bar",
             simpleDat 12 "baz",
             simpleDat 123 "quux"
             ],
          "sort" = Just Asc
          }
        }
     ]
  }
                    

funnel2 id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOption options true

  return unit
