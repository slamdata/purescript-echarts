module ECharts.Style.Link where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


import ECharts.Color

data LinkType = LTCurve | LTLine

instance linkTypeShow :: Show LinkType where
  show LTCurve = "curve"
  show LTLine = "line"

instance linkTypeEncodeJson :: EncodeJson LinkType where
  encodeJson = encodeJson <<< show



newtype LinkStyle =
  LinkStyle {
    "type" :: Maybe LinkType,
    color :: Maybe Color,
    width :: Maybe Number
  }

instance linkStyleEncodeJson :: EncodeJson LinkStyle where
  encodeJson (LinkStyle ls) =
    fromObject $ fromList $
    [
      "type" := ls.type,
      "color" := ls.color,
      "width" := ls.width
    ]
