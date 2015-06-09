module ECharts.Item.Value where

import Data.Maybe
import Data.Array (length)
import Data.Either (Either(..))
import Control.Alt ((<|>))
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Argonaut.Decode

type XYRRec = {x :: Number, y :: Number, r :: Maybe Number}
type HLOCRec = {h :: Number, l :: Number, o :: Number, c :: Number}

data ItemValue = None
               | Simple Number
               | Many [Number]
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
        case arr of 
          o:c:l:h:[] -> pure $ HLOC {h: h, l: l, o: o, c: c}
          x:y:r:[] -> pure $ XYR {x: x, y: y, r: Just r}
          x:y:[] -> pure $ XYR {x: x, y: y, r: Nothing}
          nums -> pure $ Many nums
    ) <|>
    (Simple <$> decodeJson json)  <|>
    (if decodeJson json == Right "-"
     then pure None
     else Left "Incorrect ItemValue")
