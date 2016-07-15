## Module ECharts.Style.Area

#### `AreaStyleRec`

``` purescript
type AreaStyleRec = { color :: Maybe CalculableColor, type :: Maybe String }
```

#### `AreaStyle`

``` purescript
newtype AreaStyle
  = AreaStyle AreaStyleRec
```

##### Instances
``` purescript
EncodeJson AreaStyle
DecodeJson AreaStyle
```

#### `areaStyleDefault`

``` purescript
areaStyleDefault :: AreaStyleRec
```


