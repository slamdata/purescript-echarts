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

#### `LinearGradient`

``` purescript
data LinearGradient :: *
```

#### `CalculableColor`

``` purescript
data CalculableColor
  = SimpleColor Color
  | ColorFunc (String -> Color)
  | GradientColor LinearGradient
```

##### Instances
``` purescript
EncodeJson CalculableColor
DecodeJson CalculableColor
```


