## Module ECharts.Color

#### `Color`

``` purescript
type Color = String
```

#### `ColorFuncParamRec`

``` purescript
type ColorFuncParamRec = { seriesIndex :: Number, series :: String, dataIndex :: Number, data :: { value :: ItemValue, name :: String } }
```

#### `ColorFuncParam`

``` purescript
newtype ColorFuncParam
  = ColorFuncParam ColorFuncParamRec
```

#### `CalculableColor`

``` purescript
data CalculableColor
  = SimpleColor Color
  | ColorFunc (ColorFuncParam -> Color)
```

##### Instances
``` purescript
EncodeJson CalculableColor
DecodeJson CalculableColor
```


