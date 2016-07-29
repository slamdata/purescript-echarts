## Module ECharts.Types.Phantom

#### `I`

``` purescript
data I :: !
```

Phantom effect for DSL

#### `PositionedI`

``` purescript
type PositionedI i = (left :: I, right :: I, bottom :: I, top :: I | i)
```

Note that open rows type synonims is for mixins and
closed rows are for complete dsls

#### `LegendI`

``` purescript
type LegendI = PositionedI (show :: I, items :: I, orient :: I, align :: I)
```

#### `TooltipI`

``` purescript
type TooltipI = (show :: I, showContent :: I, trigger :: I, showDelay :: I, axisPointer :: I, zlevel :: I, formatter :: I, animation :: I)
```

#### `TitleI`

``` purescript
type TitleI = (text :: I, left :: I, textStyle :: I, offsetCenter :: I, show :: I, subtext :: I)
```

#### `OptionI`

``` purescript
type OptionI = (tooltip :: I, grid :: I, xAxis :: I, yAxis :: I, color :: I, series :: I, legend :: I, title :: I, backgroundColor :: I, brush :: I, toolbox :: I, visualMap :: I)
```

#### `VisualMapI`

``` purescript
type VisualMapI = (continuousVisualMap :: I, piecewiseVisualMap :: I)
```

#### `ContinuousVisualMapI`

``` purescript
type ContinuousVisualMapI = PositionedI (dimension :: I, textPair :: I, inverse :: I, itemHeight :: I, calculable :: I, min :: I, max :: I, inRange :: I, outOfRange :: I, controller :: I)
```

#### `InOutRangeI`

``` purescript
type InOutRangeI = (color :: I, colorLightness :: I)
```

#### `ControllerI`

``` purescript
type ControllerI = (inRange :: I, outOfRange :: I)
```

#### `ToolboxI`

``` purescript
type ToolboxI = PositionedI (feature :: I, show :: I, orient :: I)
```

#### `FeatureI`

``` purescript
type FeatureI = (brush :: I, saveAsImage :: I, restore :: I, dataView :: I, dataZoom :: I, magicType :: I)
```

#### `SaveAsImageI`

``` purescript
type SaveAsImageI = (imageType :: I, name :: I, backgroundColor :: I, excludeComponents :: I, show :: I, title :: I, icon :: I, iconStyle :: I, pixelRatio :: I)
```

#### `RestoreI`

``` purescript
type RestoreI = (show :: I, title :: I, icon :: I, iconStyle :: I)
```

#### `DataViewI`

``` purescript
type DataViewI = (show :: I, title :: I, icon :: I, iconStyle :: I, readOnly :: I)
```

#### `MagicTypeI`

``` purescript
type MagicTypeI = (show :: I, magics :: I)
```

#### `MagicsI`

``` purescript
type MagicsI = (magic :: I)
```

#### `BrushI`

``` purescript
type BrushI = (brushToolbox :: I, xAxisIndex :: I)
```

#### `BrushToolboxI`

``` purescript
type BrushToolboxI = (tool :: I)
```

#### `GridI`

``` purescript
type GridI = PositionedI (show :: I, textStyle :: I)
```

#### `SeriesI`

``` purescript
type SeriesI = (pie :: I, line :: I, bar :: I, scatter :: I, effectScatter :: I, radar :: I, treeMap :: I, boxPlot :: I, candlestick :: I, heatMap :: I, map :: I, parallel :: I, lines :: I, graph :: I, sankey :: I, funnel :: I, gauge :: I)
```

There is no common serie thing, but special cases for
every kind of series.

#### `AxisI`

``` purescript
type AxisI i = (axisType :: I, items :: I, axisTick :: I, axisLabel :: I, name :: I, scale :: I, boundaryGap :: I, silent :: I, splitLine :: I, splitArea :: I, axisLine :: I, inverse :: I | i)
```

xAxis and yAxis has different position type

#### `SplitAreaI`

``` purescript
type SplitAreaI = (show :: I)
```

#### `AxisLineI`

``` purescript
type AxisLineI = (onZero :: I, lineStyle :: I)
```

#### `XAxisI`

``` purescript
type XAxisI = AxisI (horizontalPosition :: I)
```

#### `YAxisI`

``` purescript
type YAxisI = AxisI (verticalPosition :: I)
```

#### `LineSeriesI`

``` purescript
type LineSeriesI = (name :: I, xAxisIndex :: I, yAxisIndex :: I, polarIndex :: I, symbol :: I, symbolSize :: I, lineStyle :: I, itemStyle :: I, areaStyle :: I, smooth :: I, items :: I, hoverAnimation :: I, showSymbol :: I)
```

#### `BarSeriesI`

``` purescript
type BarSeriesI = (name :: I, items :: I, stack :: I, itemStyle :: I)
```

#### `PieSeriesI`

``` purescript
type PieSeriesI = (name :: I, center :: I, radius :: I, items :: I, startAngle :: I, selectedMode :: I, label :: I)
```

#### `ScatterI`

``` purescript
type ScatterI = (name :: I, items :: I, large :: I, symbolSize :: I)
```

#### `EffectScatterI`

``` purescript
type EffectScatterI = (name :: I)
```

#### `RadarI`

``` purescript
type RadarI = (name :: I)
```

#### `TreeMapI`

``` purescript
type TreeMapI = (name :: I)
```

#### `BoxPlotI`

``` purescript
type BoxPlotI = (name :: I)
```

#### `CandlestickI`

``` purescript
type CandlestickI = (name :: I)
```

#### `HeatMapI`

``` purescript
type HeatMapI = (name :: I)
```

#### `MapI`

``` purescript
type MapI = (name :: I)
```

#### `ParallelI`

``` purescript
type ParallelI = (name :: I)
```

#### `LinesI`

``` purescript
type LinesI = (name :: I)
```

#### `GraphI`

``` purescript
type GraphI = (name :: I)
```

#### `SankeyI`

``` purescript
type SankeyI = (name :: I)
```

#### `FunnelI`

``` purescript
type FunnelI = (name :: I, left :: I, width :: I, label :: I, labelLine :: I, items :: I, itemStyle :: I, maxSize :: I)
```

#### `GaugeI`

``` purescript
type GaugeI = (name :: I, z :: I, min :: I, max :: I, splitNumber :: I, gaugeRadius :: I, axisLine :: I, axisTick :: I, splitLine :: I, title :: I, detail :: I, items :: I, center :: I, endAngle :: I, gaugePointer :: I, startAngle :: I, axisLabel :: I)
```

#### `GaugePointerI`

``` purescript
type GaugePointerI = (show :: I, length :: I, width :: I)
```

#### `ItemI`

``` purescript
type ItemI = (name :: I, value :: I, symbol :: I, symbolSize :: I, symbolRotate :: I, symbolOffset :: I, label :: I, itemStyle :: I, selected :: I)
```

#### `AxisPointerI`

``` purescript
type AxisPointerI = (show :: I, pointerType :: I, lineStyle :: I, zlevel :: I)
```

#### `LineStyleI`

``` purescript
type LineStyleI = (lineType :: I, width :: I, color :: I)
```

#### `SplitLineI`

``` purescript
type SplitLineI = (show :: I, lineStyle :: I, length :: I)
```

#### `LabelI`

``` purescript
type LabelI = (normalLabel :: I, emphasisLabel :: I)
```

#### `LabelInnerI`

``` purescript
type LabelInnerI = (show :: I, textStyle :: I, position :: I, formatter :: I, color :: I)
```

#### `ItemsI`

``` purescript
type ItemsI = (item :: I)
```

#### `ItemStyleI`

``` purescript
type ItemStyleI = (normalItemStyle :: I, emphasisItemStyle :: I)
```

#### `IStyleI`

``` purescript
type IStyleI = (barBorderWidth :: I, shadowBlur :: I, shadowOffsetX :: I, shadowOffsetY :: I, shadowColor :: I, opacity :: I, borderWidth :: I, borderColor :: I)
```

#### `TextStyleI`

``` purescript
type TextStyleI = (color :: I, fontWeight :: I, fontSize :: I, fontStyle :: I)
```

#### `AreaStyleI`

``` purescript
type AreaStyleI = (color :: I)
```

#### `AxisTickI`

``` purescript
type AxisTickI = (show :: I, length :: I, lineStyle :: I, splitNumber :: I)
```

#### `AxisLabelI`

``` purescript
type AxisLabelI = (show :: I, formatter :: I)
```

#### `DetailI`

``` purescript
type DetailI = (textStyle :: I, show :: I)
```

#### `LabelLineI`

``` purescript
type LabelLineI = (normalLabelLine :: I, emphasisLabelLine :: I)
```

#### `LabelLineInnerI`

``` purescript
type LabelLineInnerI = (show :: I)
```


