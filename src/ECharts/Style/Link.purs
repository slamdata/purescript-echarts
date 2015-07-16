module ECharts.Style.Link where

import Prelude
import Data.Maybe
import Data.Either
import Data.StrMap (fromList, StrMap (..))
import Data.List (toList)
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators


import ECharts.Color

data LinkType = LTCurve | LTLine

instance linkTypeEncodeJson :: EncodeJson LinkType where
  encodeJson a = fromString $ case a of 
    LTCurve -> "curve"
    LTLine -> "line"


instance linkTypeDecodeJson :: DecodeJson LinkType where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "curve" -> pure LTCurve
      "line" -> pure LTLine
      _ -> Left "incorrect link type"

type LinkStyleRec = {
    "type" :: Maybe LinkType,
    color :: Maybe Color,
    width :: Maybe Number
  }


newtype LinkStyle = LinkStyle LinkStyleRec


instance linkStyleEncodeJson :: EncodeJson LinkStyle where
  encodeJson (LinkStyle ls) =
    fromObject $ fromList $ toList 
    [
      "type" := ls.type,
      "color" := ls.color,
      "width" := ls.width
    ]

instance linkStyleDecodeJson :: DecodeJson LinkStyle where 
  decodeJson j = do
    o <- decodeJson j
    r <- { "type": _
         , color: _
         , width: _ } <$>
         (o .? "type") <*>
         (o .? "color") <*>
         (o .? "width")
    pure $ LinkStyle r

linkStyleDefault :: LinkStyleRec
linkStyleDefault = {
  "type": Nothing,
  color: Nothing,
  width: Nothing
  }
