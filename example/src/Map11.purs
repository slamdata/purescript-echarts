module Map11 where

import Prelude
import Control.Monad.Eff.Console (print)
import Data.Tuple.Nested
import Data.Maybe
import Data.StrMap (fromList)
import Data.Tuple
import Data.List (toList)

import Utils
import Control.Monad.Eff

import ECharts.Chart
import ECharts.Options
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import ECharts.Title
import ECharts.Tooltip
import ECharts.Mark.Point
import ECharts.Mark.Data
import ECharts.Symbol

nameValue {name, value} =
  MarkPointData $ markPointDataDefault {
    name = Just name,
    value = Just value
    }

option = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerItem},
  series = Just $ Just <$> [
    MapSeries {
       common: universalSeriesDefault{
          markPoint = Just $
            MarkPoint $ markPointDefault {
              "data" = Just $ nameValue <$> [
                 {name: "trololo", value: 123.0}
                 ]
              }

          },


       mapSeries: mapSeriesDefault{
         "data" = Just [],
         mapType = Just "china",
         geoCoord = Just $ fromList $ toList [
           Tuple "trololo" (Tuple 121.0 43.0)
           ]
         }
       }
    ]
  }
map11 id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in map11"
    Just el -> init Nothing el >>= setOption option true >>= \_ -> return unit
