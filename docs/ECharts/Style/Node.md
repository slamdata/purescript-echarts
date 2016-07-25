## Module ECharts.Style.Node

#### `NodeStyleRec`

``` purescript
type NodeStyleRec = { color :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number }
```

#### `NodeStyle`

``` purescript
newtype NodeStyle
  = NodeStyle NodeStyleRec
```

##### Instances
``` purescript
EncodeJson NodeStyle
DecodeJson NodeStyle
```

#### `nodeStyleDefault`

``` purescript
nodeStyleDefault :: NodeStyleRec
```


