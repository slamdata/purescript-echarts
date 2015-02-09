module ECharts.Symbol (
  Symbol(..),
  SymbolSize(..),
  DoubleSymbolSize(..)
  ) where

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Maybe

import Data.Function
import Data.Tuple

import ECharts.Item.Data
import ECharts.Item.Value


foreign import func2json """
function func2json(fn) {
  return fn;
}
""" :: forall a. a -> Json

data Symbol = Circle | Rectangle | Triangle | Diamond | EmptyCircle | EmptyRectangle
            | EmptyTriangle | EmptyDiamond 

instance encodeJsonSymbol :: EncodeJson Symbol where
  encodeJson a = fromString $ case a of 
    Circle -> "circle"
    Rectangle -> "rectangle"
    Triangle -> "triangle"
    Diamond -> "diamond"
    EmptyCircle -> "emptyCircle"
    EmptyRectangle -> "emptyRectangle"
    EmptyTriangle -> "emptyTriangle"
    EmptyDiamond -> "emptyDiamond"

data SymbolSize = Size Number | Func (ItemValue -> Number)

instance symbolSizeEncodeJson :: EncodeJson SymbolSize where
  encodeJson ss = case ss of
    Size num -> encodeJson num
    Func func -> func2json $ mkFn1 func


data DoubleSymbolSize = DblSize (Tuple Number Number)
                      | DblFunc (ItemValue -> Tuple Number Number)

instance dblSymbolSizeEncodeJson :: EncodeJson DoubleSymbolSize where
  encodeJson ss = case ss of
    DblSize num -> encodeJson num
    DblFunc func -> func2json $ mkFn1 func


