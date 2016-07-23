module Chord2 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

options ∷ E.Option
options =
  E.Option E.optionDefault
    { series = Just $ map Just
        [ E.ChordSeries
          { common: E.universalSeriesDefault { name = Just "chord" }
          , chordSeries: E.chordSeriesDefault
              { sort = Just E.Asc
              , sortSub = Just E.Desc
              , showScale = Just true
              , showScaleText = Just true
              , "data" = Just
                  [ E.Label "group1"
                  , E.Label "group2"
                  , E.Label "group3"
                  , E.Label "group4"
                  ]
              , matrix = Just
                  [ [11975.0,  5871.0, 8916.0, 2868.0]
                  , [ 1951.0, 10048.0, 2060.0, 6171.0]
                  , [ 8010.0, 16145.0, 8090.0, 8045.0]
                  , [ 1013.0,   990.0,  940.0, 6907.0]
                  ]
              }
          }
        ]
    }


chord2 ∷ ∀ e. ElementId → Eff (dom ∷ DOM, echarts ∷ E.ECHARTS, console ∷ CONSOLE|e) Unit
chord2 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "Incorrect id in chord 2"
    Just el → E.init Nothing el >>= E.setOption options true >>= \_ -> pure unit
