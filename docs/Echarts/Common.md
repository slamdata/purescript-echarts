## Module ECharts.Common

#### `GeoCoord`

``` purescript
type GeoCoord = StrMap (Tuple Number Number)
```

#### `Corner`

``` purescript
data Corner a
  = AllCorners a
  | Corners a a a a
```

##### Instances
``` purescript
instance cornerJsonEncode :: (EncodeJson a) => EncodeJson (Corner a)
instance cornerJsonDecode :: (DecodeJson a) => DecodeJson (Corner a)
```

#### `PercentOrPixel`

``` purescript
data PercentOrPixel
  = Percent Number
  | Pixel Number
```

##### Instances
``` purescript
instance percentOrPixelEncodeJson :: EncodeJson PercentOrPixel
instance percentOrPixelDecodeJson :: DecodeJson PercentOrPixel
```

#### `RoseType`

``` purescript
data RoseType
  = RTRadius
  | RTArea
```

##### Instances
``` purescript
instance roseTypeEncodeJson :: EncodeJson RoseType
instance roseTypeDecodeJson :: DecodeJson RoseType
```

#### `SelectedMode`

``` purescript
data SelectedMode
  = SelModeSingle
  | SelModeMultiple
  | SelModeFalse
```

##### Instances
``` purescript
instance selModeEncodeJson :: EncodeJson SelectedMode
instance selModeDecodeJson :: DecodeJson SelectedMode
```

#### `MapValueCalculation`

``` purescript
data MapValueCalculation
  = SumCalculation
  | AverageCalculation
```

##### Instances
``` purescript
instance mapValueCalculationEncodeJson :: EncodeJson MapValueCalculation
instance mapValueCalculationDecodeJson :: DecodeJson MapValueCalculation
```

#### `Roam`

``` purescript
data Roam
  = Enable
  | Disable
  | Scale
  | Move
```

##### Instances
``` purescript
instance roamEncodeJson :: EncodeJson Roam
instance roamDecodeJson :: DecodeJson Roam
```

#### `MinMaxRec`

``` purescript
type MinMaxRec = { min :: Number, max :: Number }
```

#### `MinMax`

``` purescript
newtype MinMax
  = MinMax MinMaxRec
```

##### Instances
``` purescript
instance minMaxEncodeJson :: EncodeJson MinMax
instance minMaxDecodeJson :: DecodeJson MinMax
```

#### `Center`

``` purescript
type Center = Tuple PercentOrPixel PercentOrPixel
```

#### `RsRec`

``` purescript
type RsRec = { inner :: PercentOrPixel, outer :: PercentOrPixel }
```

#### `Radius`

``` purescript
data Radius
  = R PercentOrPixel
  | Rs RsRec
```

##### Instances
``` purescript
instance radiusEncodeJson :: EncodeJson Radius
instance radiusDecodeJson :: DecodeJson Radius
```

#### `Sort`

``` purescript
data Sort
  = NoSort
  | Asc
  | Desc
```

##### Instances
``` purescript
instance sortEncodeJson :: EncodeJson Sort
instance sortDecodeJson :: DecodeJson Sort
```

#### `Interval`

``` purescript
data Interval
  = Auto
  | Custom Number
```

##### Instances
``` purescript
instance intervalEncodeJson :: EncodeJson Interval
instance intervalDecodeJson :: DecodeJson Interval
```


