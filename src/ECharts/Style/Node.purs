module ECharts.Style.Node where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


import ECharts.Color

newtype NodeStyle =
  NodeStyle {
    color :: Maybe Color,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number
  }

instance nodeStyleEncodeJson :: EncodeJson NodeStyle where
  encodeJson (NodeStyle ns) =
    fromObject $ fromList $
    [
      "color" := ns.color,
      "borderColor" := ns.borderColor,
      "borderWidth" := ns.borderWidth
    ]
  
