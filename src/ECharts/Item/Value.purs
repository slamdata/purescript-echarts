module ECharts.Item.Value where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

data ItemValue = None
               | Simple Number
               | Many [Number]
               | XYR {x :: Number, y :: Number, r :: Maybe Number}
               | HLOC {h :: Number, l :: Number, o :: Number, c :: Number}

instance itemValueEncodeJson :: EncodeJson ItemValue where
  encodeJson val = case val of
    None -> encodeJson "-"
    Simple num -> encodeJson num
    Many nums -> encodeJson nums
    XYR {x = x, y = y, r = r} -> encodeJson
                                 [encodeJson x, encodeJson y, encodeJson r]
    HLOC {h = h, l = l, o = o, c = c} -> encodeJson [o, c, l, h]

  
