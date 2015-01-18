module ECharts.Mark.Data where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

newtype MarkPointData =
  MarkPointData {
    name :: Maybe String,
    value :: Maybe Number,
    x :: Maybe Number,
    y :: Maybe Number,
    xAxis :: Maybe Number,
    yAxis :: Maybe Number,
    "type" :: Maybe String
  }

instance mpDataEncodeJson :: EncodeJson MarkPointData where
  encodeJson (MarkPointData mp) =
    fromObject $ fromList $
    [
      "name" := mp.name,
      "value" := mp.value,
      "x" := mp.x,
      "y" := mp.y,
      "xAxis" := mp.xAxis,
      "yAxis" := mp.yAxis,
      "type" := mp.type
    ]

emptyMPData =
  {
    name: Nothing,
    value: Nothing,
    x: Nothing,
    y: Nothing,
    xAxis: Nothing,
    yAxis: Nothing,
    "type": Nothing
  }
