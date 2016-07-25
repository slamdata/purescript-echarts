module Gauge4 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Random (random, RANDOM)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Signal (Signal, runSignal, (~>))
import Signal.Time as ST

import Utils as U

gaugeValueSignal ∷ ∀ e. Signal (Eff (random ∷ RANDOM|e) Number)
gaugeValueSignal =
  ST.every 2000.0 ~> const do
    fst ← random
    pure $ U.precise 2.0 (fst * 100.0)

options_ ∷ Number → E.Option
options_ val =
  E.Option E.optionDefault
    { series = Just $ map Just
      [ E.GaugeSeries
          { common: E.universalSeriesDefault
          , gaugeSeries: E.gaugeSeriesDefault
              { detail = Just $ E.GaugeDetail E.gaugeDetailDefault
                  { formatter = Just $ E.Template "{value}%" }
              , "data" = Just [E.Value $ E.Simple val]
              }
          }
      ]
    }


options ∷ ∀ e. Signal (Eff (random ∷ RANDOM|e) E.Option)
options = gaugeValueSignal ~> \g → do
  gv ← g
  pure $ options_ gv


gauge4
  ∷ ∀ e
  . ElementId
  → Eff (console ∷ CONSOLE, random ∷ RANDOM, dom ∷ DOM, echarts ∷ E.ECHARTS|e) Unit
gauge4 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in gauge4"
    Just el → do
      chart ← E.init Nothing el
      runSignal $ options ~> \opts → do
        os ← opts
        E.setOption os true chart
        pure unit
