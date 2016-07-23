module Funnel2 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

simpleDat ∷ Number → String → E.ItemData
simpleDat val nam =
  E.Dat $ (E.dataDefault $ E.Simple val) {name = Just nam}

options ∷ E.Option
options =
  E.Option E.optionDefault
    { series = Just $ map Just
        [ E.FunnelSeries
            { common: E.universalSeriesDefault,
              funnelSeries: E.funnelSeriesDefault
                { "data" = Just
                    [ simpleDat 60.0 "foo"
                    , simpleDat 80.0 "bar"
                    , simpleDat 12.0 "baz"
                    , simpleDat 123.0 "quux"
                    ]
                , sort = Just E.Asc
                }
            }
        ]
    }


funnel2 ∷ ∀ e. ElementId → Eff (console ∷ CONSOLE, dom ∷ DOM, echarts ∷ E.ECHARTS|e) Unit
funnel2 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in funnel2"
    Just el → E.init Nothing el >>= E.setOption options true >>= \_ -> pure unit
