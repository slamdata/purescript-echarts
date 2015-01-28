module Gauge4 where

import Control.Monad.Eff
import Control.Monad.Eff.Random
--import Data.Tuple
import Data.Tuple.Nested
import Signal
import Signal.Time
import Data.Maybe
import Utils



import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Item.Value
import ECharts.Item.Data

gaugeValueSignal =
  every 2000 ~> const do
    fst <- random
    return (fst * 100)

options_ val = Option $ optionDefault {
  series = Just $ Just <$> [
     GaugeSeries {
        common: universalSeriesDefault,
        special: gaugeSeriesDefault{
          "data" = Just [Value $ Simple val]
          }
        }
     ]
  }


options = gaugeValueSignal ~> \g -> do
  gv <- g
  return $ options_ gv

gauge4 id = do
  chart <- getElementById id
           >>= init Nothing
  runSignal $ options ~> \opts -> do
    os <- opts
    setOption os true chart
    return unit
           
