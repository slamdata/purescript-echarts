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

instance linetypeShow :: Show LineType where
  show lt = case lt of
    Solid -> "solid"
    Dotted -> "dotted"
    Dashed -> "dashed"

instance linetypeEncodeJson :: EncodeJson LineType where
  encodeJson = encodeJson <<< show


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
