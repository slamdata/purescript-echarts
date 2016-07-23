## Module ECharts.Formatter

#### `FormatParams`

``` purescript
type FormatParams = Json
```

#### `Formatter`

``` purescript
data Formatter
  = Template String
  | FormatFunc (Array FormatParams -> String)
  | StringFormatFunc (String -> String)
  | NumberFormatFunc (Number -> String)
```

##### Instances
``` purescript
EncodeJson Formatter
DecodeJson Formatter
```


