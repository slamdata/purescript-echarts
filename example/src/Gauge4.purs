module Gauge4 where

import Prelude
import Control.Monad.Eff.Console (print)
import Control.Monad.Eff
import Control.Monad.Eff.Random
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
import ECharts.Series.Gauge
import ECharts.Formatter

gaugeValueSignal =
  every 2000.0 ~> const do
    fst <- random
    return $ precise 2.0 (fst * 100.0)

options_ val = Option $ optionDefault {
  series = Just $ Just <$> [
     GaugeSeries {
        common: universalSeriesDefault,
        gaugeSeries: gaugeSeriesDefault{
          detail = Just $ GaugeDetail gaugeDetailDefault {
             formatter = Just $ Template "{value}%"
             },
          "data" = Just [Value $ Simple val]
          }
        }
     ]
  }


options = gaugeValueSignal ~> \g -> do
  gv <- g
  return $ options_ gv


gauge4 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in gauge4"
    Just el -> do
      chart <- init Nothing el
      runSignal $ options ~> \opts -> do
        os <- opts
        setOption os true chart
        return unit

