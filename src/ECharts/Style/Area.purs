module ECharts.Style.Area where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (CalculableColor)


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
    encodeJson
      $ SM.fromFoldable
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
