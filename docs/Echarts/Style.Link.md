## Module ECharts.Style.Link

#### `LinkType`

``` purescript
data LinkType
  = LTCurve
  | LTLine
```

##### Instances
``` purescript
EncodeJson LinkType
DecodeJson LinkType
```

#### `LinkStyleRec`

``` purescript
type LinkStyleRec = { type :: Maybe LinkType, color :: Maybe Color, width :: Maybe Number }
```

#### `LinkStyle`

``` purescript
newtype LinkStyle
  = LinkStyle LinkStyleRec
```

##### Instances
``` purescript
EncodeJson LinkStyle
DecodeJson LinkStyle
```

#### `linkStyleDefault`

``` purescript
linkStyleDefault :: LinkStyleRec
```


