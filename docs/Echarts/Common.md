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
(EncodeJson a) => EncodeJson (Corner a)
(DecodeJson a) => DecodeJson (Corner a)
```

#### `PercentOrPixel`

``` purescript
data PercentOrPixel
  = Percent Number
  | Pixel Number
```

##### Instances
``` purescript
EncodeJson PercentOrPixel
DecodeJson PercentOrPixel
```

#### `RoseType`

``` purescript
data RoseType
  = RTRadius
  | RTArea
```

##### Instances
``` purescript
EncodeJson RoseType
DecodeJson RoseType
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
EncodeJson SelectedMode
DecodeJson SelectedMode
```

#### `MapValueCalculation`

``` purescript
data MapValueCalculation
  = SumCalculation
  | AverageCalculation
```

##### Instances
``` purescript
EncodeJson MapValueCalculation
DecodeJson MapValueCalculation
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
EncodeJson Roam
DecodeJson Roam
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
EncodeJson MinMax
DecodeJson MinMax
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
EncodeJson Radius
DecodeJson Radius
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
EncodeJson Sort
DecodeJson Sort
```

#### `Interval`

``` purescript
data Interval
  = Auto
  | Custom Number
```

##### Instances
``` purescript
EncodeJson Interval
DecodeJson Interval
```


