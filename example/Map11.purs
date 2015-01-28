module Map11 where

import Data.Tuple.Nested
import Data.Maybe
import Data.StrMap (fromList)
import Data.Tuple

import Utils
import Control.Monad.Eff

import ECharts.Chart
import ECharts.Options
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Type
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

nameValue {name = name, value = value} =
  MarkPointData $ markPointDataDefault {
    "name" = Just name,
    "value" = Just value
    }

-- it works, but 

option = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerItem},
  series = Just $ Just <$> [
    MapSeries {
       common: universalSeriesDefault{
          "markPoint" = Just $
            MarkPoint $ markPointDefault {
              "data" = Just $ nameValue <$> [
                 {name: "trololo", value: 123}
                 ]
              }
            
          },
       

       special: mapSeriesDefault{
         "data" = Just [],
         "mapType" = Just "china",
         "geoCoord" = Just $ fromList [
           Tuple "trololo" (Tuple 121 43)
           ]
         }
       }
    ]
  }
map11 id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOption option true

  return unit



