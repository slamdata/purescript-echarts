module ECharts.Item.Value where

import Prelude
import Data.Maybe
import Data.Either (Either(..))
import Control.Alt ((<|>))
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.List (toList, fromList, List(..))

type XYRRec = {x :: Number, y :: Number, r :: Maybe Number}
type HLOCRec = {h :: Number, l :: Number, o :: Number, c :: Number}

data ItemValue = None
               | Simple Number
               | Many (Array Number)
               | XYR XYRRec
               | HLOC HLOCRec

instance itemValueEncodeJson :: EncodeJson ItemValue where
  encodeJson val = case val of
    None -> encodeJson "-"
    Simple num -> encodeJson num
    Many nums -> encodeJson nums
    XYR {x = x, y = y, r = r} -> encodeJson
                                 [encodeJson x, encodeJson y, encodeJson r]
    HLOC {h = h, l = l, o = o, c = c} -> encodeJson [o, c, l, h]


instance itemValueDecodeJson :: DecodeJson ItemValue where
  decodeJson json =
    (do arr <- decodeJson json
        case toList (arr :: Array Number) of
          (Cons o (Cons c (Cons l (Cons h _)))) -> pure $ HLOC {h: h, l: l, o: o, c: c}
          (Cons x (Cons y (Cons r Nil))) -> pure $ XYR {x: x, y: y, r: Just r}
          (Cons x (Cons y Nil)) -> pure $ XYR {x: x, y: y, r: Nothing}
          nums -> pure $ Many (fromList nums)
    ) <|>
    (Simple <$> decodeJson json)  <|>
    (if decodeJson json == Right "-"
     then pure None
     else Left "Incorrect ItemValue")
