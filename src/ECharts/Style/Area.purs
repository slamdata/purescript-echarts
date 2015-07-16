module ECharts.Style.Area where

import Prelude
import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.List (toList)

import ECharts.Color

newtype AreaStyle = AreaStyle Color

instance areaStyleEncodeJson :: EncodeJson AreaStyle where
  encodeJson (AreaStyle color) =
    fromObject $ fromList $ toList
    [
      "color" := color,
      "type" := "fill"
    ]

instance areaStyleDecodeJson :: DecodeJson AreaStyle where
  decodeJson j = do
    o <- decodeJson j
    AreaStyle <$> (o .? "color") 
