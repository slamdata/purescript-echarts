## Module ECharts.Types.Phantom

#### `I`

``` purescript
data I :: !
```

Phantom effect for DSL

#### `R`

``` purescript
data R :: # ! -> !
```

Takes row of effects and returns an effect. Useful for emulating structural polymorphism.

#### `PositionMixin`

``` purescript
type PositionMixin i = (left :: I, right :: I, bottom :: I, top :: I | i)
```

There is no some row constraints because they have no sense
E.g. `coordinateSystem` field is fully determined by
`xAxisIndex/yAxisIndex/polarIndex/radarIndex/etc`
Open rows synonims are for mixins
closed rows are for complete dsls

#### `ZMixin`

``` purescript
type ZMixin i = (zlevel :: I, z :: I | i)
```

#### `SizeMixin`

``` purescript
type SizeMixin i = (width :: I, height :: I | i)
```

#### `BorderAndBackgroundMixin`

``` purescript
type BorderAndBackgroundMixin i = (borderWidth :: I, borderColor :: I, backgroundColor :: I | i)
```

#### `ShadowMixin`

``` purescript
type ShadowMixin i = (shadowBlur :: I, shadowOffsetX :: I, shadowOffsetY :: I, shadowColor :: I | i)
```

#### `BaseAnimationMixin`

``` purescript
type BaseAnimationMixin i = (animationDuration :: I, animationEasing :: I, animationDelay :: I | i)
```

#### `LargeMixin`

``` purescript
type LargeMixin i = (large :: I, largeThreshold :: I | i)
```

#### `AnimationMixin`

``` purescript
type AnimationMixin i = BaseAnimationMixin (animation :: I, animationThreshold :: I, animationDurationUpdate :: I, animationEasingUpdate :: I, animationDelayUpdate :: I | i)
```

#### `MarkerMixin`

``` purescript
type MarkerMixin i = (markPoint :: I, markLine :: I, markArea :: I | i)
```

#### `SymbolMixin`

``` purescript
type SymbolMixin i = (symbol :: I, symbolSize :: I, symbolRotate :: I, symbolOffset :: I | i)
```

#### `MinMaxMixin`

``` purescript
type MinMaxMixin i = (min :: I, max :: I | i)
```

#### `NameStyleMixin`

``` purescript
type NameStyleMixin i = (nameLocation :: I, nameTextStyle :: I, nameGap :: I, nameRotate :: I | i)
```

#### `LegendHoverMixin`

``` purescript
type LegendHoverMixin i = (legendHoverLink :: I, hoverAnimation :: I | i)
```

#### `NormalAndEmphasis`

``` purescript
type NormalAndEmphasis i = (normal :: R i, emphasis :: R i)
```

#### `LegendI`

``` purescript
type LegendI = PositionMixin (ZMixin (SizeMixin (ShadowMixin (BorderAndBackgroundMixin (show :: I, items :: I, orient :: I, align :: I, padding :: I, itemGap :: I, itemWidth :: I, itemHeight :: I, formatter :: I, inactiveColor :: I, selected :: I, textStyle :: I, selectedMode :: I, tooltip :: I)))))
```

#### `TooltipI`

``` purescript
type TooltipI = ZMixin (BorderAndBackgroundMixin (show :: I, showContent :: I, trigger :: I, triggerOn :: I, alwaysShowContent :: I, showDelay :: I, hideDelay :: I, enterable :: I, tooltipPosition :: I, transitionDuration :: I, formatter :: I, animation :: I, textStyle :: I, padding :: I, extraCssText :: I, axisPointer :: I))
```

#### `CrossStyleI`

``` purescript
type CrossStyleI = (color :: I, width :: I, lineType :: I, textStyle :: I)
```

#### `TitleI`

``` purescript
type TitleI = PositionMixin (ZMixin (ShadowMixin (BorderAndBackgroundMixin (text :: I, textStyle :: I, offsetCenter :: I, show :: I, subtext :: I, link :: I, target :: I, textAlign :: I, textBaseLine :: I, sublink :: I, subtarget :: I, subtextStyle :: I, padding :: I, itemGap :: I))))
```

#### `OptionI`

``` purescript
type OptionI = AnimationMixin (tooltip :: I, grid :: I, xAxis :: I, yAxis :: I, color :: I, series :: I, legend :: I, title :: I, backgroundColor :: I, brush :: I, toolbox :: I, visualMap :: I, radar :: I, polar :: I, radiusAxis :: I, angleAxis :: I, dataZoom :: I, geo :: I, parallel :: I, parallelAxis :: I, singleAxis :: I, timeline :: I, textStyle :: I, progressive :: I, progressiveThreshold :: I, blendMode :: I, hoverLayerThreshold :: I)
```

#### `TimelineI`

``` purescript
type TimelineI = PositionMixin (ZMixin (SymbolMixin (show :: I, timelineType :: I, axisType :: I, currentIndex :: I, autoPlay :: I, rewind :: I, loop :: I, playInterval :: I, realtime :: I, controlPosition :: I, orient :: I, inverse :: I, lineStyle :: I, label :: I, itemStyle :: I, checkpointStyle :: I, controlStyle :: I, items :: I)))
```

#### `ParallelI`

``` purescript
type ParallelI = PositionMixin (ZMixin (SizeMixin (layout :: I, axisExpandable :: I, axisExpandCenter :: I, axisExpandCount :: I, axisExpandWidth :: I, parallelAxisDefault :: I)))
```

#### `GeoI`

``` purescript
type GeoI = PositionMixin (ZMixin (show :: I, map :: I, roam :: I, center :: I, zoom :: I, scaleLimit :: I, nameMap :: I, selectedMode :: I, label :: I, itemStyle :: I, layoutCenter :: I, layoutSize :: I, regions :: I, silent :: I))
```

#### `PolarI`

``` purescript
type PolarI = ZMixin (center :: I, radius :: I)
```

#### `RadarI`

``` purescript
type RadarI = ZMixin (indicators :: I, shape :: I, splitNumber :: I, radarName :: I, splitLine :: I, splitArea :: I, axisLine :: I, center :: I, radius :: I, startAngle :: I, nameGap :: I, scale :: I, silent :: I, triggerEvent :: I, axisTick :: I, axisLabel :: I)
```

#### `IndicatorsI`

``` purescript
type IndicatorsI = (indicator :: I)
```

#### `IndicatorI`

``` purescript
type IndicatorI = MinMaxMixin (name :: I)
```

#### `RadarNameI`

``` purescript
type RadarNameI = (show :: I, formatter :: I, textStyle :: I)
```

#### `DataZoomI`

``` purescript
type DataZoomI = (insideDataZoom :: I, sliderDataZoom :: I)
```

#### `DataZoomMixinI`

``` purescript
type DataZoomMixinI i = (xAxisIndex :: I, yAxisIndex :: I, filterMode :: I, start :: I, end :: I, startValue :: I, endValue :: I, orient :: I, zoomLock :: I, throttle :: I | i)
```

#### `InsideDataZoomI`

``` purescript
type InsideDataZoomI = DataZoomMixinI ()
```

#### `SliderDataZoom`

``` purescript
type SliderDataZoom = PositionMixin (ZMixin (show :: I, backgroundColor :: I, dataBackground :: I, fillerColor :: I, borderColor :: I, handleIcon :: I, handleSize :: I, handleStyle :: I, labelPrecision :: I, labelFormatter :: I, showDetail :: I, showDataShadow :: I, realtime :: I, textStyle :: I))
```

#### `DataBackgroundI`

``` purescript
type DataBackgroundI = (lineStyle :: I, areaStyle :: I)
```

#### `HandleStyleI`

``` purescript
type HandleStyleI = ShadowMixin (color :: I, borderColor :: I, borderWidth :: I, borderType :: I, opacity :: I)
```

#### `VisualMapI`

``` purescript
type VisualMapI = (continuousVisualMap :: I, piecewiseVisualMap :: I)
```

#### `ContinuousVisualMapI`

``` purescript
type ContinuousVisualMapI = PositionMixin (ZMixin (BorderAndBackgroundMixin (MinMaxMixin (dimension :: I, textPair :: I, inverse :: I, itemHeight :: I, calculable :: I, inRange :: I, outOfRange :: I, controller :: I, orient :: I, range :: I, realtime :: I, precision :: I, itemWidth :: I, align :: I, textGap :: I, show :: I, seriesIndex :: I, hoverLink :: I, padding :: I, color :: I, textStyle :: I, formatter :: I))))
```

#### `PiecewiseVisualMapI`

``` purescript
type PiecewiseVisualMapI = PositionMixin (ZMixin (BorderAndBackgroundMixin (MinMaxMixin (splitNumber :: I, pieces :: I, categories :: I, selectedMode :: I, inverse :: I, precision :: I, itemWidth :: I, itemHeight :: I, align :: I, text :: I, textGap :: I, itemGap :: I, itemSymbol :: I, show :: I, dimension :: I, seriesIndex :: I, hoverLink :: I, outOfRange :: I, inRange :: I, controller :: I, orient :: I, padding :: I, color :: I, textStyle :: I, formatter :: I))))
```

#### `InOutRangeI`

``` purescript
type InOutRangeI = (color :: I, symbol :: I, symbolSize :: I, colorAlpha :: I, opacity :: I, colorSaturation :: I, colorHue :: I, colorLightness :: I)
```

#### `ControllerI`

``` purescript
type ControllerI = (inRange :: I, outOfRange :: I)
```

#### `ToolboxI`

``` purescript
type ToolboxI = PositionMixin (ZMixin (SizeMixin (feature :: I, show :: I, orient :: I, itemSize :: I, itemGap :: I, iconStyle :: I)))
```

#### `IconStyleI`

``` purescript
type IconStyleI = NormalAndEmphasis IconStyleInnerI
```

#### `IconStyleInnerI`

``` purescript
type IconStyleInnerI = ShadowMixin (color :: I, borderColor :: I, borderWidth :: I, borderType :: I, opacity :: I)
```

#### `FeatureI`

``` purescript
type FeatureI = (brush :: I, saveAsImage :: I, restore :: I, dataView :: I, dataZoom :: I, magicType :: I)
```

#### `DataZoomFeatureI`

``` purescript
type DataZoomFeatureI = (show :: I, dzfTitle :: I, dzfIcon :: I, iconStyle :: I, xAxisIndex :: I, yAxisIndex :: I)
```

#### `DZFI`

``` purescript
type DZFI = (zoom :: I, back :: I)
```

#### `SaveAsImageI`

``` purescript
type SaveAsImageI = (imageType :: I, name :: I, backgroundColor :: I, excludeComponents :: I, show :: I, title :: I, icon :: I, iconStyle :: I, pixelRatio :: I)
```

#### `RestoreI`

``` purescript
type RestoreI = (show :: I, title :: I, readOnly :: I, optionToContent :: I, contentToOption :: I, lang :: I, backgroundColor :: I, textareaColor :: I, textareaBorderColor :: I, textColor :: I, buttonColor :: I, buttonTextColor :: I, icon :: I, iconStyle :: I)
```

#### `DataViewI`

``` purescript
type DataViewI = (show :: I, title :: I, icon :: I, iconStyle :: I, readOnly :: I)
```

#### `MagicTypeI`

``` purescript
type MagicTypeI = (show :: I, mtTitle :: I, mtIcon :: I, mtOption :: I, mtSeriesIndex :: I, magics :: I)
```

#### `MTFieldI`

``` purescript
type MTFieldI = (line :: I, bar :: I, stack :: I, tiled :: I)
```

#### `BrushFeatureI`

``` purescript
type BrushFeatureI = (brushType :: I, bfIcon :: I, bfTitle :: I)
```

#### `BFFieldI`

``` purescript
type BFFieldI = (rect :: I, polygon :: I, lineX :: I, lineY :: I, keep :: I, clear :: I)
```

#### `MagicsI`

``` purescript
type MagicsI = (magic :: I)
```

#### `BrushI`

``` purescript
type BrushI = (brushToolbox :: I, xAxisIndex :: I, brushLink :: I, seriesIndex :: I, geoIndex :: I, yAxisIndex :: I, brushType :: I, brushMode :: I, transformable :: I, brushStyle :: I, throttleType :: I, throttleDelay :: I, removeOnClick :: I, inBrush :: I, outOfBrush :: I)
```

#### `BrushToolboxI`

``` purescript
type BrushToolboxI = (tool :: I)
```

#### `GridI`

``` purescript
type GridI = PositionMixin (ZMixin (SizeMixin (ShadowMixin (BorderAndBackgroundMixin (show :: I, textStyle :: I, containLabel :: I)))))
```

#### `SeriesI`

``` purescript
type SeriesI = (pie :: I, line :: I, bar :: I, scatter :: I, effectScatter :: I, radarSeries :: I, treeMap :: I, boxPlot :: I, candlestick :: I, heatMap :: I, map :: I, parallelSeries :: I, lines :: I, graph :: I, sankey :: I, funnel :: I, gauge :: I, missing :: I)
```

There is no common serie thing, but special cases for
every kind of series.

#### `AxisI`

``` purescript
type AxisI i = ZMixin (MinMaxMixin (axisType :: I, items :: I, axisTick :: I, axisLabel :: I, name :: I, scale :: I, boundaryGap :: I, silent :: I, splitLine :: I, splitArea :: I, axisLine :: I, interval :: I, inverse :: I, splitNumber :: I, minInterval :: I, triggerEvent :: I | i))
```

xAxis and yAxis has different position type

#### `SplitAreaI`

``` purescript
type SplitAreaI = (show :: I, interval :: I, areaStyle :: I)
```

#### `AxisLineI`

``` purescript
type AxisLineI = (show :: I, onZero :: I, lineStyle :: I)
```

#### `YAxesI`

``` purescript
type YAxesI = (addYAxis :: I)
```

#### `XAxesI`

``` purescript
type XAxesI = (addXAxis :: I)
```

#### `ParallelAxisI`

``` purescript
type ParallelAxisI = AxisI (NameStyleMixin (dim :: I, parallelIndex :: I, realtime :: I))
```

#### `XAxisI`

``` purescript
type XAxisI = AxisI (NameStyleMixin (horizontalPosition :: I, gridIndex :: I, offset :: I))
```

#### `YAxisI`

``` purescript
type YAxisI = AxisI (NameStyleMixin (verticalPosition :: I, gridIndex :: I, offset :: I))
```

#### `RadiusAxisI`

``` purescript
type RadiusAxisI = AxisI (NameStyleMixin (polarIndex :: I))
```

#### `AngleAxisI`

``` purescript
type AngleAxisI = AxisI (polarIndex :: I, startAngle :: I, clockwise :: I)
```

#### `SingleAxisI`

``` purescript
type SingleAxisI = AxisI (NameStyleMixin ())
```

#### `LineSeriesI`

``` purescript
type LineSeriesI = ZMixin (AnimationMixin (MarkerMixin (SymbolMixin (LegendHoverMixin (name :: I, xAxisIndex :: I, yAxisIndex :: I, polarIndex :: I, showSymbol :: I, showAllSymbol :: I, connectNulls :: I, clipOverflow :: I, step :: I, lineStylePair :: I, itemStyle :: I, areaStylePair :: I, smooth :: I, smoothMonotone :: I, sampling :: I, items :: I, stack :: I, silent :: I, label :: I)))))
```

#### `BarSeriesI`

``` purescript
type BarSeriesI = ZMixin (AnimationMixin (MarkerMixin (name :: I, items :: I, stack :: I, legendHoverLink :: I, xAxisIndex :: I, yAxisIndex :: I, itemStyle :: I, label :: I, barWidth :: I, barMaxWidth :: I, barMinHeight :: I, barGap :: I, barCategoryGap :: I)))
```

#### `PieSeriesI`

``` purescript
type PieSeriesI = ZMixin (AnimationMixin (LegendHoverMixin (name :: I, center :: I, radius :: I, items :: I, startAngle :: I, selectedMode :: I, selectedOffset :: I, clockwise :: I, minAngle :: I, roseType :: I, avoidLabelOverlap :: I, label :: I, labelLine :: I, itemStyle :: I, silent :: I)))
```

#### `BasicScatterSeriesI`

``` purescript
type BasicScatterSeriesI i = MarkerMixin (ZMixin (AnimationMixin (SymbolMixin (LargeMixin (LegendHoverMixin (name :: I, items :: I, itemStyle :: I, xAxisIndex :: I, yAxisIndex :: I, polarIndex :: I, geoIndex :: I, label :: I, silent :: I | i))))))
```

#### `ScatterI`

``` purescript
type ScatterI = BasicScatterSeriesI ()
```

#### `EffectScatterI`

``` purescript
type EffectScatterI = BasicScatterSeriesI (effectType :: I, showEffectOn :: I, rippleEffect :: I)
```

#### `RadarSeriesI`

``` purescript
type RadarSeriesI = ZMixin (AnimationMixin (SymbolMixin (name :: I, items :: I, itemStyle :: I, lineStylePair :: I, areaStylePair :: I, axisLine :: I, radarIndex :: I, label :: I)))
```

#### `TreeMapI`

``` purescript
type TreeMapI = PositionMixin (ZMixin (SizeMixin (BaseAnimationMixin (squareRatio :: I, leafDepth :: I, roam :: I, nodeClick :: I, zoomNodeToRatio :: I, levels :: I, silentTreeMap :: I, visualDimensions :: I, colorAlpha :: I, colorSaturation :: I, colorMappingBy :: I, visibleMin :: I, childrenVisibleMin :: I, label :: I, itemStyle :: I, breadcrumb :: I, items :: I))))
```

#### `BoxPlotI`

``` purescript
type BoxPlotI = ZMixin (BaseAnimationMixin (MarkerMixin (LegendHoverMixin (xAxisIndex :: I, yAxisIndex :: I, name :: I, layout :: I, boxWidth :: I, itemStyle :: I, items :: I, silent :: I))))
```

#### `CandlestickI`

``` purescript
type CandlestickI = ZMixin (BaseAnimationMixin (MarkerMixin (LegendHoverMixin (name :: I, items :: I, xAxisIndex :: I, yAxisIndex :: I, layout :: I, itemStyle :: I))))
```

#### `HeatMapI`

``` purescript
type HeatMapI = ZMixin (MarkerMixin (name :: I, xAxisIndex :: I, yAxisIndex :: I, geoIndex :: I, blurSize :: I, minOpacity :: I, maxOpacity :: I, items :: I, label :: I, itemStyle :: I, silent :: I))
```

#### `MapI`

``` purescript
type MapI = PositionMixin (MarkerMixin (ZMixin (name :: I, roam :: I, map :: I, center :: I, zoom :: I, scaleLimit :: I, nameMap :: I, selectedMode :: I, label :: I, itemStyle :: I, layoutCenter :: I, layoutSize :: I, mapValueCalculation :: I, showLegendSymbol :: I, silent :: I)))
```

#### `ScaleLimitI`

``` purescript
type ScaleLimitI = MinMaxMixin ()
```

#### `RegionsI`

``` purescript
type RegionsI = (region :: I)
```

#### `RegionI`

``` purescript
type RegionI = (name :: I, selected :: I, itemStyle :: I, label :: I)
```

#### `ParallelSeriesI`

``` purescript
type ParallelSeriesI = AnimationMixin (ZMixin (parallelIndex :: I, name :: I, lineStyle :: I, inactiveOpacity :: I, activeOpacity :: I, realtime :: I, items :: I, silent :: I))
```

#### `LinesI`

``` purescript
type LinesI = ZMixin (AnimationMixin (MarkerMixin (LargeMixin (name :: I, xAxisIndex :: I, yAxisIndex :: I, geoIndex :: I, polyline :: I, effect :: I, lineStyle :: I, label :: I, items :: I, silent :: I))))
```

#### `GraphI`

``` purescript
type GraphI = AnimationMixin (ZMixin (PositionMixin (SizeMixin (SymbolMixin (MarkerMixin (LegendHoverMixin (name :: I, xAxisIndex :: I, yAxisIndex :: I, polarIndex :: I, geoIndex :: I, layout :: I, force :: I, roam :: I, nodeScaleRatio :: I, draggable :: I, focusNodeAdjancency :: I, itemStyle :: I, lineStylePair :: I, label :: I, categories :: I, items :: I, nodes :: I, links :: I, edges :: I, edgeLabel :: I, edgeSymbols :: I, edgeSymbolSize :: I, silent :: I)))))))
```

#### `EdgeLabelI`

``` purescript
type EdgeLabelI = NormalAndEmphasis EdgeLabelInnerI
```

#### `EdgeLabelInnerI`

``` purescript
type EdgeLabelInnerI = (show :: I, edgeLabelPosition :: I, formatter :: I, textStyle :: I)
```

#### `LinksI`

``` purescript
type LinksI = (link :: I)
```

#### `LinkI`

``` purescript
type LinkI = (source :: I, target :: I, label :: I, symbol :: I, lineStylePair :: I, symbolSize :: I)
```

#### `EdgeSymbolsI`

``` purescript
type EdgeSymbolsI = (edgeSymbol :: I)
```

#### `SankeyI`

``` purescript
type SankeyI = AnimationMixin (ZMixin (SizeMixin (PositionMixin (nodeWidth :: I, nodeGap :: I, layoutIterations :: I, label :: I, itemStyle :: I, lineStyle :: I, items :: I, nodes :: I, links :: I, edges :: I, silent :: I))))
```

#### `FunnelI`

``` purescript
type FunnelI = AnimationMixin (MarkerMixin (PositionMixin (SizeMixin (MinMaxMixin (name :: I, label :: I, labelLine :: I, items :: I, itemStyle :: I, minSize :: I, maxSize :: I, sort :: I, gap :: I, legendHoverLink :: I, funnelAlign :: I, silent :: I)))))
```

#### `GaugeI`

``` purescript
type GaugeI = AnimationMixin (ZMixin (MarkerMixin (MinMaxMixin (name :: I, gaugeRadius :: I, center :: I, radius :: I, startAngle :: I, endAngle :: I, clockwise :: I, splitNumber :: I, axisLine :: I, splitLine :: I, axisTick :: I, axisLabel :: I, gaugePointer :: I, itemStyle :: I, items :: I, title :: I, detail :: I, silent :: I))))
```

#### `GaugePointerI`

``` purescript
type GaugePointerI = (show :: I, length :: I, width :: I)
```

#### `ItemI`

``` purescript
type ItemI = SymbolMixin (name :: I, value :: I, label :: I, itemStyle :: I, selected :: I, icon :: I, x :: I, y :: I)
```

#### `AxisPointerI`

``` purescript
type AxisPointerI = AnimationMixin (show :: I, axis :: I, pointerType :: I, axisPointerType :: I, shadowStyle :: I, lineStyle :: I, crossStyle :: I)
```

#### `ShadowStyleI`

``` purescript
type ShadowStyleI = ShadowMixin (color :: I, opacity :: I)
```

#### `LineStylePairI`

``` purescript
type LineStylePairI = NormalAndEmphasis LineStyleI
```

#### `LineStyleI`

``` purescript
type LineStyleI = ShadowMixin (lineType :: I, width :: I, color :: I, opacity :: I, curveness :: I)
```

#### `SplitLineI`

``` purescript
type SplitLineI = (show :: I, lineStyle :: I, interval :: I, length :: I)
```

#### `LabelI`

``` purescript
type LabelI = NormalAndEmphasis LabelInnerI
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
type ItemStyleI = NormalAndEmphasis IStyleI
```

#### `IStyleI`

``` purescript
type IStyleI = ShadowMixin (barBorderWidth :: I, opacity :: I, borderWidth :: I, borderColor :: I, color :: I)
```

#### `TextStyleI`

``` purescript
type TextStyleI = (color :: I, fontWeight :: I, fontSize :: I, fontStyle :: I, fontFamily :: I)
```

#### `AreaStylePairI`

``` purescript
type AreaStylePairI = NormalAndEmphasis AreaStyleI
```

#### `AreaStyleI`

``` purescript
type AreaStyleI = ShadowMixin (color :: I, opacity :: I)
```

#### `AxisTickI`

``` purescript
type AxisTickI = (show :: I, length :: I, inside :: I, interval :: I, alignWithLabel :: I, lineStyle :: I, splitNumber :: I)
```

#### `AxisLabelI`

``` purescript
type AxisLabelI = (show :: I, formatter :: I, interval :: I, inside :: I, margin :: I, rotate :: I, textStyle :: I)
```

#### `DetailI`

``` purescript
type DetailI = (textStyle :: I, show :: I)
```

#### `LabelLineI`

``` purescript
type LabelLineI = NormalAndEmphasis LabelLineInnerI
```

#### `LabelLineInnerI`

``` purescript
type LabelLineInnerI = (show :: I)
```

#### `ValuesI`

``` purescript
type ValuesI = (addValue :: I)
```

#### `MarkPointI`

``` purescript
type MarkPointI = AnimationMixin (SymbolMixin (items :: I, label :: I, itemStyle :: I, silent :: I))
```

#### `MarkLineI`

``` purescript
type MarkLineI = AnimationMixin (SymbolMixin (precision :: I, label :: I, silent :: I, lineStylePair :: I, items :: I))
```

#### `MarkAreaI`

``` purescript
type MarkAreaI = AnimationMixin (label :: I, itemStyle :: I, items :: I)
```

#### `RippleEffectI`

``` purescript
type RippleEffectI = (period :: I, scale :: I, brushType :: I)
```

#### `LevelsI`

``` purescript
type LevelsI = (level :: I)
```

#### `LevelI`

``` purescript
type LevelI = (visualDimensions :: I, color :: I, colorAlpha :: I, colorSaturation :: I, colorMappingBy :: I, visibleMin :: I, childrenVisibleMin :: I, label :: I, itemStyle :: I)
```

#### `EffectI`

``` purescript
type EffectI = (show :: I, period :: I, constantSpeed :: I, symbol :: I, symbolSize :: I, color :: I, trailLength :: I, loop :: I)
```

#### `ForceI`

``` purescript
type ForceI = (initLayout :: I, repulsion :: I, gravity :: I, edgeLength :: I, layoutAnimation :: I)
```

#### `CategoriesI`

``` purescript
type CategoriesI = (category :: I)
```

#### `CategoryI`

``` purescript
type CategoryI = SymbolMixin (name :: I, itemStyle :: I, label :: I)
```


