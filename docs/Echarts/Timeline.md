## Module ECharts.Timeline

#### `TimelineType`

``` purescript
data TimelineType
  = TimelineTime
  | TimelineNumber
```

##### Instances
``` purescript
instance timelineTypeEncodeJson :: EncodeJson TimelineType
instance timelineTypeDecodeJson :: DecodeJson TimelineType
```

#### `TimelineControlPosition`

``` purescript
data TimelineControlPosition
  = TCPLeft
  | TCPRight
  | TCPNone
```

##### Instances
``` purescript
instance timelineControlPositionEncodeJson :: EncodeJson TimelineControlPosition
instance timelineControlPositionDecodeJson :: DecodeJson TimelineControlPosition
```

#### `TimelineRec`

``` purescript
type TimelineRec = { show :: Maybe Boolean, type :: Maybe TimelineType, notMerge :: Maybe Boolean, realtime :: Maybe Boolean, x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, padding :: Maybe (Corner Number), controlPosition :: Maybe TimelineControlPosition, autoPlay :: Maybe Boolean, loop :: Maybe Boolean, playInterval :: Maybe Number, lineStyle :: Maybe LineStyle, label :: Maybe AxisLabel, checkpointStyle :: Maybe CheckpointStyle, controlStyle :: Maybe ItemStyle, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, currentIndex :: Maybe Number, data :: Maybe (Array String) }
```

#### `Timeline`

``` purescript
newtype Timeline
  = Timeline TimelineRec
```

##### Instances
``` purescript
instance timelineEncodeJson :: EncodeJson Timeline
instance timelineDecodeJson :: DecodeJson Timeline
```

#### `timelineDefault`

``` purescript
timelineDefault :: TimelineRec
```


