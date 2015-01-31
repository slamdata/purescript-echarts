module ECharts.Item.Value where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


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

  
