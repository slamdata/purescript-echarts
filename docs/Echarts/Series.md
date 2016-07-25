## Module ECharts.Series

#### `Series`

``` purescript
data Series
  = LineSeries { common :: UniversalSeriesRec, lineSeries :: LineSeriesRec }
  | BarSeries { common :: UniversalSeriesRec, barSeries :: BarSeriesRec }
  | ScatterSeries { common :: UniversalSeriesRec, scatterSeries :: ScatterSeriesRec }
  | CandlestickSeries { common :: UniversalSeriesRec, candlestickSeries :: CandlestickSeriesRec }
  | PieSeries { common :: UniversalSeriesRec, pieSeries :: PieSeriesRec }
  | RadarSeries { common :: UniversalSeriesRec, radarSeries :: RadarSeriesRec }
  | ChordSeries { common :: UniversalSeriesRec, chordSeries :: ChordSeriesRec }
  | ForceSeries { common :: UniversalSeriesRec, forceSeries :: ForceSeriesRec }
  | MapSeries { common :: UniversalSeriesRec, mapSeries :: MapSeriesRec }
  | GaugeSeries { common :: UniversalSeriesRec, gaugeSeries :: GaugeSeriesRec }
  | FunnelSeries { common :: UniversalSeriesRec, funnelSeries :: FunnelSeriesRec }
  | EventRiverSeries { common :: UniversalSeriesRec, eventRiverSeries :: EventRiverSeriesRec }
```

##### Instances
``` purescript
EncodeJson Series
DecodeJson Series
```

#### `UniversalSeriesRec`

``` purescript
type UniversalSeriesRec = { name :: Maybe String, tooltip :: Maybe Tooltip, clickable :: Maybe Boolean, itemStyle :: Maybe ItemStyle, markPoint :: Maybe MarkPoint, markLine :: Maybe MarkLine }
```

#### `universalSeriesDefault`

``` purescript
universalSeriesDefault :: UniversalSeriesRec
```

#### `LineSeriesRec`

``` purescript
type LineSeriesRec = { data :: Maybe (Array ItemData), stack :: Maybe String, xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, showAllSymbol :: Maybe Boolean, smooth :: Maybe Boolean, legendHoverLink :: Maybe Boolean }
```

#### `lineSeriesDefault`

``` purescript
lineSeriesDefault :: LineSeriesRec
```

#### `BarSeriesRec`

``` purescript
type BarSeriesRec = { data :: Maybe (Array ItemData), stack :: Maybe String, xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, barGap :: Maybe PercentOrPixel, barCategoryGap :: Maybe PercentOrPixel, barMinHeight :: Maybe Number, barWidth :: Maybe Number, barMaxWidth :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `barSeriesDefault`

``` purescript
barSeriesDefault :: BarSeriesRec
```

#### `ScatterSeriesRec`

``` purescript
type ScatterSeriesRec = { data :: Maybe (Array ItemData), xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, large :: Maybe Boolean, largeThreshold :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `scatterSeriesDefault`

``` purescript
scatterSeriesDefault :: ScatterSeriesRec
```

#### `CandlestickSeriesRec`

``` purescript
type CandlestickSeriesRec = { data :: Maybe (Array ItemData), xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, barMinHeight :: Maybe Number, barWidth :: Maybe Number, barMaxWidth :: Maybe Number }
```

#### `candlestickSeriesDefault`

``` purescript
candlestickSeriesDefault :: CandlestickSeriesRec
```

#### `PieSeriesRec`

``` purescript
type PieSeriesRec = { data :: Maybe (Array ItemData), center :: Maybe Center, radius :: Maybe Radius, startAngle :: Maybe Number, minAngle :: Maybe Number, clockWise :: Maybe Boolean, roseType :: Maybe RoseType, selectedOffset :: Maybe Number, selectedMode :: Maybe SelectedMode, legendHoverLink :: Maybe Boolean }
```

#### `pieSeriesDefault`

``` purescript
pieSeriesDefault :: PieSeriesRec
```

#### `RadarSeriesRec`

``` purescript
type RadarSeriesRec = { data :: Maybe (Array ItemData), polarIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, legendHoverLink :: Maybe Boolean }
```

#### `radarSeriesDefault`

``` purescript
radarSeriesDefault :: RadarSeriesRec
```

#### `ChordSeriesRec`

``` purescript
type ChordSeriesRec = { nodes :: Maybe (Array Node), categories :: Maybe (Array ForceCategory), links :: Maybe (Array Link), matrix :: Maybe Matrix, data :: Maybe (Array ItemData), ribbonType :: Maybe Boolean, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, minRadius :: Maybe Number, maxRadius :: Maybe Number, showScale :: Maybe Boolean, showScaleText :: Maybe Boolean, padding :: Maybe Number, sort :: Maybe Sort, sortSub :: Maybe Sort, clockWise :: Maybe Boolean }
```

#### `chordSeriesDefault`

``` purescript
chordSeriesDefault :: ChordSeriesRec
```

#### `ForceSeriesRec`

``` purescript
type ForceSeriesRec = { categories :: Maybe (Array ForceCategory), nodes :: Maybe (Array Node), links :: Maybe (Array Link), matrix :: Maybe Matrix, center :: Maybe Center, size :: Maybe Number, minRadius :: Maybe Number, maxRadius :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, linkSymbol :: Maybe Symbol, linkSymbolSize :: Maybe Symbol, scaling :: Maybe Number, gravity :: Maybe Number, draggable :: Maybe Number, large :: Maybe Boolean, useWorker :: Maybe Boolean, steps :: Maybe Number, ribbonType :: Maybe Boolean }
```

#### `forceSeriesDefault`

``` purescript
forceSeriesDefault :: ForceSeriesRec
```

#### `MapSeriesRec`

``` purescript
type MapSeriesRec = { data :: Maybe (Array ItemData), selectedMode :: Maybe SelectedMode, mapType :: Maybe String, hoverable :: Maybe Boolean, dataRangeHoverLink :: Maybe Boolean, mapLocation :: Maybe Location, mapValueCalculation :: Maybe MapValueCalculation, mapValuePrecision :: Maybe Number, showLegendSymbol :: Maybe Boolean, roam :: Maybe Roam, scaleLimit :: Maybe MinMax, nameMap :: Maybe (StrMap String), textFixed :: Maybe (StrMap (Tuple Number Number)), geoCoord :: Maybe (StrMap (Tuple Number Number)) }
```

#### `mapSeriesDefault`

``` purescript
mapSeriesDefault :: MapSeriesRec
```

#### `GaugeSeriesRec`

``` purescript
type GaugeSeriesRec = { data :: Maybe (Array ItemData), center :: Maybe (Tuple Number Number), radius :: Maybe Radius, startAngle :: Maybe Number, endAngle :: Maybe Number, min :: Maybe Number, max :: Maybe Number, precision :: Maybe Number, splitNumber :: Maybe Number, axisLine :: Maybe AxisLine, axisTick :: Maybe AxisTick, axisLabel :: Maybe AxisLabel, splitLine :: Maybe SplitLine, title :: Maybe Title, detail :: Maybe GaugeDetail, pointer :: Maybe Pointer, legendHoverLink :: Maybe Boolean }
```

#### `gaugeSeriesDefault`

``` purescript
gaugeSeriesDefault :: GaugeSeriesRec
```

#### `FunnelSeriesRec`

``` purescript
type FunnelSeriesRec = { data :: Maybe (Array ItemData), x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, funnelAlign :: Maybe HorizontalAlign, min :: Maybe Number, max :: Maybe Number, minSize :: Maybe PercentOrPixel, maxSize :: Maybe PercentOrPixel, gap :: Maybe Number, sort :: Maybe Sort, legendHoverLink :: Maybe Boolean }
```

#### `funnelSeriesDefault`

``` purescript
funnelSeriesDefault :: FunnelSeriesRec
```

#### `EventRiverSeriesRec`

``` purescript
type EventRiverSeriesRec = { eventList :: Maybe (Array OneEvent), xAxisIndex :: Maybe Number, weight :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `eventRiverSeriesDefault`

``` purescript
eventRiverSeriesDefault :: EventRiverSeriesRec
```

#### `setSeries`

``` purescript
setSeries :: forall e. Array Series -> Boolean -> EChart -> Eff e EChart
```


