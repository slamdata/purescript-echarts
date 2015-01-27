module ECharts.Style.Line where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import ECharts.Color

data LineType = Solid
              | Dotted
              | Dashed

instance linetypeEncodeJson :: EncodeJson LineType where
  encodeJson a = fromString $ case a of
    Solid -> "solid"
    Dotted -> "dotted"
    Dashed -> "dashed"    


newtype LineStyle =
  LineStyle {
    color :: Maybe Color,
    "type" :: Maybe LineType, 
    width :: Maybe Number,
    shadowColor :: Maybe Color,
    shadowOffsetX :: Maybe Number,
    shadowOffsetY :: Maybe Number
    }

instance lineStyleEncodeJson :: EncodeJson LineStyle where
  encodeJson (LineStyle ls) =
    fromObject $ fromList $
    [
      "color" := ls.color,
      "type" := ls.type,
      "width" := ls.width,
      "shadowColor" := ls.shadowColor,
      "shadowOffsetX" := ls.shadowOffsetX,
      "shadowOffsetY" := ls.shadowOffsetY
    ]


lineStyleDefault = {
  color: Nothing,
  type: Nothing,
  width: Nothing,
  shadowColor: Nothing,
  shadowOffsetX: Nothing,
  shadowOffsetY: Nothing
  }
