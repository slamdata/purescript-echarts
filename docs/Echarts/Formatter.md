## Module ECharts.Formatter

#### `FormatParams`

``` purescript
type FormatParams = Json
```

#### `GenericFormatter`

``` purescript
data GenericFormatter :: *
```

#### `Formatter`

``` purescript
data Formatter
  = Template String
  | FormatFunc (forall eff. Array FormatParams -> Eff eff String)
  | StringFormatFunc (String -> String)
  | NumberFormatFunc (Number -> String)
  | GenericFormatFunc GenericFormatter
```

##### Instances
``` purescript
EncodeJson Formatter
DecodeJson Formatter
```


