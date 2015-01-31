module ECharts.Style.Link where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


import ECharts.Color

data LinkType = LTCurve | LTLine

instance linkTypeEncodeJson :: EncodeJson LinkType where
  encodeJson a = fromString $ case a of 
    LTCurve -> "curve"
    LTLine -> "line"

type LinkStyleRec = {
    "type" :: Maybe LinkType,
    color :: Maybe Color,
    width :: Maybe Number
  }


newtype LinkStyle = LinkStyle LinkStyleRec


instance linkStyleEncodeJson :: EncodeJson LinkStyle where
  encodeJson (LinkStyle ls) =
    fromObject $ fromList $
    [
      "type" := ls.type,
      "color" := ls.color,
      "width" := ls.width
    ]

linkStyleDefault :: LinkStyleRec
linkStyleDefault = {
  "type": Nothing,
  color: Nothing,
  width: Nothing
  }
