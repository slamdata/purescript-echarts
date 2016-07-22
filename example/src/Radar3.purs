module Radar3 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

indicator ∷ String → Number → E.Indicator
indicator text max =
  E.Indicator E.indicatorDefault{text = Just text, max = Just max}

datPair ∷ Array Number → String → E.ItemData
datPair val name =
  E.Dat $ (E.dataDefault $ E.Many val) {name = Just name}

option ∷ E.Option
option =
  E.Option
    E.optionDefault
      { polar =
          Just $ [ E.Polar E.polarDefault
            { indicator =
                 Just [ indicator "sales" 6000.0
                      , indicator "Administration" 16000.0
                      , indicator "IT" 30000.0
                      , indicator "Development" 52000.0
                      , indicator "Customer Support" 38000.0
                      , indicator "Marketing" 25000.0
                      ]
            }]
      , series = Just $ map Just
          [ E.RadarSeries
              { common: E.universalSeriesDefault {name = Just "budget vs spending"}
              , radarSeries: E.radarSeriesDefault
                  { "data" =
                      Just [ datPair [4300.0, 10000.0, 28000.0, 35000.0, 50000.0, 19000.0] "Allocated"
                           , datPair [5000.0, 14000.0, 28000.0, 31000.0, 42000.0, 21000.0] "Actual"
                           ]
                  }
              }
          ]
      }

radar3 ∷ ∀ e. ElementId → Eff (console ∷ CONSOLE, dom ∷ DOM, echarts ∷ E.ECHARTS|e) Unit
radar3 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in radar3"
    Just el → do
      ch ← E.init Nothing el
      E.setOption option true ch
      pure unit
