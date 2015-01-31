module Chord2 where

import Debug.Trace (trace)
import Data.Array hiding (init)
import Data.String (indexOf, replace)
import Data.Tuple.Nested
import Data.Argonaut
import Data.Either
import Data.Maybe 
import qualified Data.StrMap as M
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
  "series" = Just $ Just <$> [
     ChordSeries {
        common: universalSeriesDefault {
           "name" = Just "chord"
           },
        chordSeries: chordSeriesDefault {
          sort = Just Asc,
          "sortSub" = Just Desc,
          "showScale" = Just true,
          "showScaleText" = Just true,
          "data" = Just [
            Label "group1",
            Label "group2",
            Label "group3",
            Label "group4"
            ],
          "matrix" = Just $  [
            [11975,  5871, 8916, 2868],
            [ 1951, 10048, 2060, 6171],
            [ 8010, 16145, 8090, 8045],
            [ 1013,   990,  940, 6907]
            ]
          }
        }
     ]
  }

chord2 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> trace "Incorrect id in chord 2"
    Just el -> init Nothing el >>= setOption options true >>= \_ -> return unit
