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
EncodeJson TextBaseline
DecodeJson TextBaseline
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
EncodeJson FontStyle
DecodeJson FontStyle
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
EncodeJson FontWeight
DecodeJson FontWeight
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
EncodeJson TextStyle
DecodeJson TextStyle
```

#### `textStyleDefault`

``` purescript
textStyleDefault :: TextStyleRec
```


