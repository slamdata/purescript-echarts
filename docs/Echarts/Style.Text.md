## Module ECharts.Style.Text

#### `Decoration`

``` purescript
type Decoration = String
```

#### `FontFamily`

``` purescript
type FontFamily = String
```

#### `TextBaseline`

``` purescript
data TextBaseline
  = TBLTop
  | TBLBottom
  | TBLMiddle
```

##### Instances
``` purescript
instance textBaselineEncodeJson :: EncodeJson TextBaseline
instance textBaselineDecodeJson :: DecodeJson TextBaseline
```

#### `FontStyle`

``` purescript
data FontStyle
  = FSNormal
  | FSItalic
  | FSOblique
```

##### Instances
``` purescript
instance fontStyleEncodeJson :: EncodeJson FontStyle
instance fontStyleDecodeJson :: DecodeJson FontStyle
```

#### `FontWeight`

``` purescript
data FontWeight
  = FWNormal
  | FWBold
  | FWBolder
  | FWLighter
  | FW100
  | FW200
  | FW300
  | FW400
  | FW500
  | FW600
  | FW700
  | FW800
  | FW900
```

##### Instances
``` purescript
instance fontWeightEncodeJson :: EncodeJson FontWeight
instance fontWeightDecodeJson :: DecodeJson FontWeight
```

#### `TextStyleRec`

``` purescript
type TextStyleRec = { color :: Maybe Color, decoration :: Maybe Decoration, align :: Maybe HorizontalAlign, baseline :: Maybe TextBaseline, fontFamily :: Maybe FontFamily, fontSize :: Maybe Number, fontStyle :: Maybe FontStyle, fontWeight :: Maybe FontWeight }
```

#### `TextStyle`

``` purescript
newtype TextStyle
  = TextStyle TextStyleRec
```

##### Instances
``` purescript
instance textStyleEncodeJson :: EncodeJson TextStyle
instance textStyleDecodeJson :: DecodeJson TextStyle
```

#### `textStyleDefault`

``` purescript
textStyleDefault :: TextStyleRec
```


