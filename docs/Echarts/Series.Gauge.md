## Module ECharts.Series.Gauge

#### `PointerRec`

``` purescript
type PointerRec = { length :: Maybe Number, width :: Maybe Number, color :: Maybe Color }
```

#### `Pointer`

``` purescript
newtype Pointer
  = Pointer PointerRec
```

##### Instances
``` purescript
instance pointerEncodeJson :: EncodeJson Pointer
instance pointerDecodeJson :: DecodeJson Pointer
```

#### `pointerDefault`

``` purescript
pointerDefault :: PointerRec
```

#### `SplitLineRec`

``` purescript
type SplitLineRec = { show :: Maybe Boolean, length :: Maybe Number, lineStyle :: Maybe LineStyle }
```

#### `SplitLine`

``` purescript
newtype SplitLine
  = SplitLine SplitLineRec
```

##### Instances
``` purescript
instance splitLineEncodeJson :: EncodeJson SplitLine
instance splitLineDecodeJson :: DecodeJson SplitLine
```

#### `splitLineDefault`

``` purescript
splitLineDefault :: SplitLineRec
```

#### `GaugeDetailRec`

``` purescript
type GaugeDetailRec = { show :: Maybe Boolean, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, width :: Maybe Number, height :: Maybe Number, offsetCenter :: Maybe (Tuple PercentOrPixel PercentOrPixel), formatter :: Maybe Formatter, textStyle :: Maybe TextStyle }
```

#### `GaugeDetail`

``` purescript
newtype GaugeDetail
  = GaugeDetail GaugeDetailRec
```

##### Instances
``` purescript
instance gaugeDetailEncodeJson :: EncodeJson GaugeDetail
instance gaugeDetailDecodeJson :: DecodeJson GaugeDetail
```

#### `gaugeDetailDefault`

``` purescript
gaugeDetailDefault :: GaugeDetailRec
```


