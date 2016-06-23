## Module ECharts.Color

#### `Color`

``` purescript
type Color = String
```

#### `ColorFuncParamRec`

``` purescript
type ColorFuncParamRec = { seriesIndex :: Maybe Number, series :: Maybe String, dataIndex :: Maybe Number, data :: { value :: Maybe ItemValue, name :: Maybe String } }
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
  | ColorFunc (String -> Color)
  | ForeignColorFunc (String -> Unit)
```

##### Instances
``` purescript
EncodeJson CalculableColor
DecodeJson CalculableColor
```


