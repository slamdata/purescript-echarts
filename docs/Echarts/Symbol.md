## Module ECharts.Symbol

#### `Symbol`

``` purescript
data Symbol
  = Circle
  | Rectangle
  | Triangle
  | Diamond
  | EmptyCircle
  | EmptyRectangle
  | EmptyTriangle
  | EmptyDiamond
```

##### Instances
``` purescript
instance encodeJsonSymbol :: EncodeJson Symbol
instance symbolDecodeJson :: DecodeJson Symbol
```

#### `SymbolSize`

``` purescript
data SymbolSize
  = Size Number
  | Func (ItemValue -> Number)
```

##### Instances
``` purescript
instance symbolSizeEncodeJson :: EncodeJson SymbolSize
instance symbolSizeDecodeJson :: DecodeJson SymbolSize
```

#### `DoubleSymbolSize`

``` purescript
data DoubleSymbolSize
  = DblSize (Tuple Number Number)
  | DblFunc (ItemValue -> Tuple Number Number)
```

##### Instances
``` purescript
instance dblSymbolSizeEncodeJson :: EncodeJson DoubleSymbolSize
instance dblSymbolSizeDecodeJson :: DecodeJson DoubleSymbolSize
```


