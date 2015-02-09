module ECharts.Style.Chord where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


import ECharts.Color

type ChordStyleRec = {
    width :: Maybe Number,
    color :: Maybe Color,
    borderWidth :: Maybe Number,
    borderColor :: Maybe Color
  }

newtype ChordStyle = ChordStyle ChordStyleRec
   


instance chordStyleJson :: EncodeJson ChordStyle where
  encodeJson (ChordStyle cs) =
    fromObject $ fromList $
    [
      "width" := cs.width,
      "color" := cs.color,
      "borderWidth" := cs.borderWidth,
      "borderColor" := cs.borderColor
    ]

chordStyleDefault :: ChordStyleRec
chordStyleDefault = {
  width: Nothing,
  color: Nothing,
  borderWidth: Nothing,
  borderColor: Nothing
  }
