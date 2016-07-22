module Funnel2 where

import Prelude
import Control.Monad.Eff.Console (print)
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
        funnelSeries: funnelSeriesDefault {
          "data" = Just $  [
             simpleDat 60.0 "foo",
             simpleDat 80.0 "bar",
             simpleDat 12.0 "baz",
             simpleDat 123.0 "quux"
             ],
          sort = Just Asc
          }
        }
     ]
  }


funnel2 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in funnel2"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit



