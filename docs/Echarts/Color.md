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

#### `LinearGradientInput`

``` purescript
type LinearGradientInput = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number, s0 :: Number, sc0 :: String, s1 :: Number, sc1 :: String }
```

#### `linearGradientInputDefault`

``` purescript
linearGradientInputDefault :: LinearGradientInput
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


