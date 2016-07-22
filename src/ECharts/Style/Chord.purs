module ECharts.Style.Chord where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (Color)

type ChordStyleRec =
  { width ∷ Maybe Number
  , color ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , borderColor ∷ Maybe Color
  }

newtype ChordStyle
  = ChordStyle ChordStyleRec

instance chordStyleJson ∷ EncodeJson ChordStyle where
  encodeJson (ChordStyle cs) =
    encodeJson
      $ SM.fromFoldable
        [ "width" := cs.width
        , "color" := cs.color
        , "borderWidth" := cs.borderWidth
        , "borderColor" := cs.borderColor
        ]

instance chordStyleDecodeJson ∷ DecodeJson ChordStyle where
  decodeJson j = do
    o ← decodeJson j
    r ← { width: _
        , color: _
        , borderWidth: _
        , borderColor: _ }
        <$> (o .? "width")
        <*> (o .? "color")
        <*> (o .? "borderWidth")
        <*> (o .? "borderColor")
    pure $ ChordStyle r

chordStyleDefault ∷ ChordStyleRec
chordStyleDefault =
  { width: Nothing
  , color: Nothing
  , borderWidth: Nothing
  , borderColor: Nothing
  }
