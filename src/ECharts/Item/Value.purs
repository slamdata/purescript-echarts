module ECharts.Item.Value where

import ECharts.Prelude

import Data.List (List(..))
import Data.List as L
import Data.Array as A

type XYRRec =
  { x ∷ Number
  , y ∷ Number
  , r ∷ Maybe Number
  }

type HLOCRec =
  { h ∷ Number
  , l ∷ Number
  , o ∷ Number
  , c ∷ Number
  }

data ItemValue
  = None
  | Simple Number
  | Many (Array Number)
  | XYR XYRRec
  | HLOC HLOCRec

instance itemValueEncodeJson ∷ EncodeJson ItemValue where
  encodeJson val = case val of
    None → encodeJson "-"
    Simple num → encodeJson num
    Many nums → encodeJson nums
    XYR {x, y, r} → encodeJson [encodeJson x, encodeJson y, encodeJson r]
    HLOC {h, l, o, c} → encodeJson [o, c, l, h]


instance itemValueDecodeJson ∷ DecodeJson ItemValue where
  decodeJson json =
    (do arr ← decodeJson json
        case L.fromFoldable (arr ∷ Array Number) of
          (Cons o (Cons c (Cons l (Cons h _)))) → pure $ HLOC {h, l, o, c}
          (Cons x (Cons y (Cons r Nil))) → pure $ XYR {x, y, r: Just r}
          (Cons x (Cons y Nil)) → pure $ XYR {x, y, r: Nothing}
          nums → pure $ Many $ A.fromFoldable nums
    )
    <|>
    (Simple <$> decodeJson json)
    <|>
    (if decodeJson json == Right "-"
     then pure None
     else Left "Incorrect ItemValue")
