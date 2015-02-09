module ECharts.Style.Area where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import ECharts.Color

newtype AreaStyle = AreaStyle Color

instance areaStyleEncodeJson :: EncodeJson AreaStyle where
  encodeJson (AreaStyle color) =
    fromObject $ fromList $
    [
      "color" := color,
      "type" := "fill"
    ]
