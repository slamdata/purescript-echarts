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
  | NoSymbol
```

##### Instances
``` purescript
EncodeJson Symbol
DecodeJson Symbol
```

#### `SymbolSize`

``` purescript
data SymbolSize
  = Size Number
  | Func (ItemValue -> Number)
```

##### Instances
``` purescript
EncodeJson SymbolSize
DecodeJson SymbolSize
```

#### `DoubleSymbolSize`

``` purescript
data DoubleSymbolSize
  = DblSize (Tuple Number Number)
  | DblFunc (ItemValue -> Tuple Number Number)
```

##### Instances
``` purescript
EncodeJson DoubleSymbolSize
DecodeJson DoubleSymbolSize
```


