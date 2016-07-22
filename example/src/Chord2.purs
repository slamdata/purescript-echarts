module Chord2 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Array hiding (init)
import Data.String (indexOf, replace)
import Data.Tuple.Nested
import Data.Either
import Data.Maybe
import Data.StrMap as M
import Control.Alt


import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Item.Data
import ECharts.Common
import ECharts.Series.Force

import Data.Maybe
import Utils


options = Option $ optionDefault {
  series = Just $ Just <$> [
     ChordSeries {
        common: universalSeriesDefault {
           name = Just "chord"
           },
        chordSeries: chordSeriesDefault {
          sort = Just Asc,
          sortSub = Just Desc,
          showScale = Just true,
          showScaleText = Just true,
          "data" = Just [
            Label "group1",
            Label "group2",
            Label "group3",
            Label "group4"
            ],
          matrix = Just $  [
            [11975.0,  5871.0, 8916.0, 2868.0],
            [ 1951.0, 10048.0, 2060.0, 6171.0],
            [ 8010.0, 16145.0, 8090.0, 8045.0],
            [ 1013.0,   990.0,  940.0, 6907.0]
            ]
          }
        }
     ]
  }

chord2 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> log "Incorrect id in chord 2"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit
