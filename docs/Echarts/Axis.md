## Module ECharts.Axis

#### `AxisLineStyleRec`

``` purescript
type AxisLineStyleRec = { color :: Maybe (Array (Tuple Number Color)), width :: Maybe Number }
```

#### `AxisLineStyle`

``` purescript
newtype AxisLineStyle
  = AxisLineStyle AxisLineStyleRec
```

##### Instances
``` purescript
instance axisLineStyleEncodeJson :: EncodeJson AxisLineStyle
instance axisLineStyleDecodeJson :: DecodeJson AxisLineStyle
```

#### `axisLineStyleDefault`

``` purescript
axisLineStyleDefault :: AxisLineStyleRec
```

#### `AxisLineRec`

``` purescript
type AxisLineRec = { show :: Maybe Boolean, onZero :: Maybe Boolean, lineStyle :: Maybe AxisLineStyle }
```

#### `AxisLine`

``` purescript
newtype AxisLine
  = AxisLine AxisLineRec
```

##### Instances
``` purescript
instance axisLineEncodeJson :: EncodeJson AxisLine
instance axisLineDecodeJson :: DecodeJson AxisLine
```

#### `axisLineDefault`

``` purescript
axisLineDefault :: AxisLineRec
```

#### `AxisTickRec`

``` purescript
type AxisTickRec = { show :: Maybe Boolean, splitNumber :: Maybe Number, length :: Maybe Number, lineStyle :: Maybe LineStyle, interval :: Maybe Interval, onGap :: Maybe Boolean, inside :: Maybe Boolean }
```

#### `AxisTick`

``` purescript
newtype AxisTick
  = AxisTick AxisTickRec
```

##### Instances
``` purescript
instance axisTickEncodeJson :: EncodeJson AxisTick
instance axisTickDecodeJson :: DecodeJson AxisTick
```

#### `axisTickDefault`

``` purescript
axisTickDefault :: AxisTickRec
```

#### `AxisLabelRec`

``` purescript
type AxisLabelRec = { show :: Maybe Boolean, interval :: Maybe Interval, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle, rotate :: Maybe Number, margin :: Maybe Number, clickable :: Maybe Boolean }
```

#### `AxisLabel`

``` purescript
newtype AxisLabel
  = AxisLabel AxisLabelRec
```

##### Instances
``` purescript
instance axisLabelEncodeJson :: EncodeJson AxisLabel
instance axisLabelDecodeJson :: DecodeJson AxisLabel
```

#### `axisLabelDefault`

``` purescript
axisLabelDefault :: AxisLabelRec
```

#### `Axises`

``` purescript
data Axises
  = OneAxis Axis
  | TwoAxises Axis Axis
```

##### Instances
``` purescript
instance axisesEncodeJson :: EncodeJson Axises
instance axisesDecodeJson :: DecodeJson Axises
```

#### `AxisSplitLineRec`

``` purescript
type AxisSplitLineRec = { show :: Maybe Boolean, onGap :: Maybe Boolean, lineStyle :: Maybe LineStyle }
```

#### `AxisSplitLine`

``` purescript
newtype AxisSplitLine
  = AxisSplitLine AxisSplitLineRec
```

##### Instances
``` purescript
instance axisSplitLineEncodeJson :: EncodeJson AxisSplitLine
instance axisSplitLineDecodeJson :: DecodeJson AxisSplitLine
```

#### `axisSplitLineDefault`

``` purescript
axisSplitLineDefault :: AxisSplitLineRec
```

#### `AxisSplitAreaRec`

``` purescript
type AxisSplitAreaRec = { show :: Maybe Boolean, onGap :: Maybe Boolean, areaStyle :: Maybe AreaStyle }
```

#### `AxisSplitArea`

``` purescript
newtype AxisSplitArea
  = AxisSplitArea AxisSplitAreaRec
```

##### Instances
``` purescript
instance axisSplitAreaEncodeJson :: EncodeJson AxisSplitArea
instance axisSplitAreaDecodeJson :: DecodeJson AxisSplitArea
```

#### `axisSplitAreaDefault`

``` purescript
axisSplitAreaDefault :: AxisSplitAreaRec
```

#### `AxisType`

``` purescript
data AxisType
  = CategoryAxis
  | ValueAxis
  | TimeAxis
```

##### Instances
``` purescript
instance axisTypeEncodeJson :: EncodeJson AxisType
instance axisTypeDecodeJson :: DecodeJson AxisType
```

#### `AxisPosition`

``` purescript
data AxisPosition
  = LeftAxis
  | RightAxis
  | TopAxis
  | BottomAxis
```

##### Instances
``` purescript
instance axisPositionEncodeJson :: EncodeJson AxisPosition
instance axisPositionDecodeJson :: DecodeJson AxisPosition
```

#### `AxisNameLocation`

``` purescript
data AxisNameLocation
  = Start
  | End
```

##### Instances
``` purescript
instance axisNameLocationEncodeJson :: EncodeJson AxisNameLocation
instance axisNameLocationDecodeJson :: DecodeJson AxisNameLocation
```

#### `CustomAxisDataRec`

``` purescript
type CustomAxisDataRec = { value :: String, textStyle :: TextStyle }
```

#### `AxisData`

``` purescript
data AxisData
  = CommonAxisData String
  | CustomAxisData CustomAxisDataRec
```

##### Instances
``` purescript
instance axisDataEncodeJson :: EncodeJson AxisData
instance axisDataDecodeJson :: DecodeJson AxisData
```

#### `AxisBoundaryGap`

``` purescript
data AxisBoundaryGap
  = CatBoundaryGap Boolean
  | ValueBoundaryGap Number Number
```

##### Instances
``` purescript
instance axisBoundaryGapEncodeJson :: EncodeJson AxisBoundaryGap
instance axisBoundaryGapDecodeJson :: DecodeJson AxisBoundaryGap
```

#### `AxisRec`

``` purescript
type AxisRec = { type :: Maybe AxisType, show :: Maybe Boolean, position :: Maybe AxisPosition, name :: Maybe String, nameLocation :: Maybe AxisNameLocation, nameTextStyle :: Maybe TextStyle, boundaryGap :: Maybe AxisBoundaryGap, min :: Maybe Number, max :: Maybe Number, scale :: Maybe Boolean, splitNumber :: Maybe Number, axisLine :: Maybe AxisLine, axisTick :: Maybe AxisTick, axisLabel :: Maybe AxisLabel, splitLine :: Maybe AxisSplitLine, splitArea :: Maybe AxisSplitArea, data :: Maybe (Array AxisData) }
```

#### `Axis`

``` purescript
newtype Axis
  = Axis AxisRec
```

##### Instances
``` purescript
instance axisEncJson :: EncodeJson Axis
instance axisDecJson :: DecodeJson Axis
```

#### `axisDefault`

``` purescript
axisDefault :: AxisRec
```

#### `PolarNameRec`

``` purescript
type PolarNameRec = { show :: Maybe Boolean, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle }
```

#### `PolarName`

``` purescript
newtype PolarName
  = PolarName PolarNameRec
```

##### Instances
``` purescript
instance polarNameEncode :: EncodeJson PolarName
instance polarNameDecodeJson :: DecodeJson PolarName
```

#### `polarNameDefault`

``` purescript
polarNameDefault :: PolarNameRec
```

#### `PolarType`

``` purescript
data PolarType
  = PolarPolygon
  | PolarCircle
```

##### Instances
``` purescript
instance polarTypeEncode :: EncodeJson PolarType
instance polarTypeDecodeJson :: DecodeJson PolarType
```

#### `IndicatorRec`

``` purescript
type IndicatorRec = { text :: Maybe String, min :: Maybe Number, max :: Maybe Number, axisLabel :: Maybe AxisLabel }
```

#### `Indicator`

``` purescript
newtype Indicator
  = Indicator IndicatorRec
```

##### Instances
``` purescript
instance indicatorEncodeJson :: EncodeJson Indicator
instance indicatorDecodeJson :: DecodeJson Indicator
```

#### `indicatorDefault`

``` purescript
indicatorDefault :: IndicatorRec
```

#### `PolarRec`

``` purescript
type PolarRec = { center :: Maybe (Tuple PercentOrPixel PercentOrPixel), radius :: Maybe PercentOrPixel, startAngle :: Maybe Number, splitNumber :: Maybe Number, name :: Maybe PolarName, boundaryGap :: Maybe (Tuple Number Number), scale :: Maybe Boolean, axisLine :: Maybe AxisLine, axisLabel :: Maybe AxisLabel, splitLine :: Maybe AxisSplitLine, splitArea :: Maybe AxisSplitArea, type :: Maybe PolarType, indicator :: Maybe (Array Indicator) }
```

#### `Polar`

``` purescript
newtype Polar
  = Polar PolarRec
```

##### Instances
``` purescript
instance polarEncodeJson :: EncodeJson Polar
instance polarDecodeJson :: DecodeJson Polar
```

#### `polarDefault`

``` purescript
polarDefault :: PolarRec
```


