module ECharts.Style.Area where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import ECharts.Color

data AreaType = Fill

instance areaTypeShow :: Show AreaType where
  show Fill = "fill"

instance areaTypeEncodeJson :: EncodeJson AreaType where
  encodeJson = encodeJson <<< show

newtype AreaStyle =
  AreaStyle {
    color :: Maybe Color,
    "type" :: Maybe AreaType
  }

instance areaStyleEncodeJson :: EncodeJson AreaStyle where
  encodeJson (AreaStyle as) =
    fromObject $ fromList $
    [
      "color" := as.color,
      "type" := as.type
    ]
