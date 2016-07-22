module ECharts.Style.Area where

import Prelude
import Data.StrMap (fromList)
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.List (toList)
import Data.Maybe

import ECharts.Color


type AreaStyleRec =
  { color ∷ Maybe CalculableColor
  , "type" ∷ Maybe String
  }

newtype AreaStyle
  = AreaStyle AreaStyleRec

areaStyleDefault ∷ AreaStyleRec
areaStyleDefault =
  { color: Nothing
  , "type": Just "fill"
  }

instance areaStyleEncodeJson ∷ EncodeJson AreaStyle where
  encodeJson (AreaStyle ars) =
    fromObject
      $ fromList
      $ toList
        [ "color" := ars.color
        , "type" := ars."type"
        ]

instance areaStyleDecodeJson ∷ DecodeJson AreaStyle where
  decodeJson j = do
    o ← decodeJson j
    r ← { color: _
        , "type": _ }
        <$> (o .? "color")
        <*> (o .? "type")
    pure $ AreaStyle r
