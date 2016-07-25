## Module ECharts.Style.Line

#### `LineType`

``` purescript
data LineType
  = Solid
  | Dotted
  | Dashed
```

##### Instances
``` purescript
EncodeJson LineType
DecodeJson LineType
```

#### `LineStyleRec`

``` purescript
type LineStyleRec = { color :: Maybe Color, type :: Maybe LineType, width :: Maybe Number, shadowColor :: Maybe Color, shadowOffsetX :: Maybe Number, shadowOffsetY :: Maybe Number }
```

#### `LineStyle`

``` purescript
newtype LineStyle
  = LineStyle LineStyleRec
```

##### Instances
``` purescript
EncodeJson LineStyle
DecodeJson LineStyle
```

#### `lineStyleDefault`

``` purescript
lineStyleDefault :: LineStyleRec
```


