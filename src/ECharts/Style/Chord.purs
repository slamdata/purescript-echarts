module ECharts.Style.Chord where

import Prelude
import Data.Maybe
import Data.StrMap (fromList)
import Data.List (toList)
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators

import ECharts.Color

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
    fromObject
      $ fromList
      $ toList
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
        <*> x(o .? "borderColor")
    pure $ ChordStyle r

chordStyleDefault ∷ ChordStyleRec
chordStyleDefault =
  { width: Nothing
  , color: Nothing
  , borderWidth: Nothing
  , borderColor: Nothing
  }
