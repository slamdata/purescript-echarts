module ECharts.Symbol
  ( Symbol(..)
  , SymbolSize(..)
  , DoubleSymbolSize(..)
  ) where

import ECharts.Prelude

import Unsafe.Coerce (unsafeCoerce)

import ECharts.Item.Value (ItemValue)


func2json ∷ ∀ a b. (a → b) → Json
func2json = unsafeCoerce

data Symbol
  = Circle
  | Rectangle
  | Triangle
  | Diamond
  | EmptyCircle
  | EmptyRectangle
  | EmptyTriangle
  | EmptyDiamond
  | NoSymbol

instance encodeJsonSymbol ∷ EncodeJson Symbol where
  encodeJson a = encodeJson $ case a of
    Circle → "circle"
    Rectangle → "rectangle"
    Triangle → "triangle"
    Diamond → "diamond"
    EmptyCircle → "emptyCircle"
    EmptyRectangle → "emptyRectangle"
    EmptyTriangle → "emptyTriangle"
    EmptyDiamond → "emptyDiamond"
    NoSymbol → "none"

instance symbolDecodeJson ∷ DecodeJson Symbol where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "circle" → pure Circle
      "rectangle" → pure Rectangle
      "triangle" → pure Triangle
      "diamond" → pure Diamond
      "emptyCircle" → pure EmptyCircle
      "emptyRectangle" → pure EmptyRectangle
      "emptyTriangle" → pure EmptyTriangle
      "emptyDiamond" → pure EmptyDiamond
      "none" → pure NoSymbol
      _ → Left "incorrect symbol"


data SymbolSize
  = Size Number
  | Func (ItemValue → Number)
  | ArrayMappingFunc (Array Number → Number)

instance symbolSizeEncodeJson ∷ EncodeJson SymbolSize where
  encodeJson = case _ of
    Size num → encodeJson num
    Func func → func2json func
    ArrayMappingFunc func → func2json func

instance symbolSizeDecodeJson ∷ DecodeJson SymbolSize where
  decodeJson j = Size <$> decodeJson j


data DoubleSymbolSize
  = DblSize (Tuple Number Number)
  | DblFunc (ItemValue → Tuple Number Number)

instance dblSymbolSizeEncodeJson ∷ EncodeJson DoubleSymbolSize where
  encodeJson = case _ of
    DblSize num → encodeJson num
    DblFunc func → func2json func

instance dblSymbolSizeDecodeJson ∷ DecodeJson DoubleSymbolSize where
  decodeJson j = DblSize <$> decodeJson j
