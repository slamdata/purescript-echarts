## Module ECharts.Style.Link

#### `LinkType`

``` purescript
data LinkType
  = LTCurve
  | LTLine
```

##### Instances
``` purescript
instance linkTypeEncodeJson :: EncodeJson LinkType
instance linkTypeDecodeJson :: DecodeJson LinkType
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
instance linkStyleEncodeJson :: EncodeJson LinkStyle
instance linkStyleDecodeJson :: DecodeJson LinkStyle
```

#### `linkStyleDefault`

``` purescript
linkStyleDefault :: LinkStyleRec
```


