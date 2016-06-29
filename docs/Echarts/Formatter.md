## Module ECharts.Formatter

#### `FormatParams`

``` purescript
type FormatParams = Json
```

#### `Formatter`

``` purescript
data Formatter
  = Template String
  | FormatFunc (forall eff. Array FormatParams -> Eff eff String)
  | ForeignFormatFunc (forall eff. Eff eff Unit)
  | F (String -> String)
```

##### Instances
``` purescript
EncodeJson Formatter
DecodeJson Formatter
```


