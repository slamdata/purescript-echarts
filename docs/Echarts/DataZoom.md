## Module ECharts.DataZoom

#### `DataZoomRec`

``` purescript
type DataZoomRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, width :: Maybe Number, height :: Maybe Number, backgroundColor :: Maybe Color, dataBackgroundColor :: Maybe Color, fillerColor :: Maybe Color, handleColor :: Maybe Color, xAxisIndex :: Maybe (Array Number), yAxisIndex :: Maybe (Array Number), start :: Maybe Number, end :: Maybe Number, showDetail :: Maybe Boolean, realtime :: Maybe Boolean, zoomlock :: Maybe Boolean }
```

#### `DataZoom`

``` purescript
newtype DataZoom
  = DataZoom DataZoomRec
```

##### Instances
``` purescript
EncodeJson DataZoom
DecodeJson DataZoom
```

#### `dataZoomDefault`

``` purescript
dataZoomDefault :: DataZoomRec
```


