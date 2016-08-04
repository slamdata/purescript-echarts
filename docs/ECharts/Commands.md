## Module ECharts.Commands

#### `series`

``` purescript
series :: forall i. DSL SeriesI -> DSL (series :: I | i)
```

#### `tooltip`

``` purescript
tooltip :: forall i. DSL TooltipI -> DSL (tooltip :: I | i)
```

#### `grid`

``` purescript
grid :: forall i. DSL GridI -> DSL (grid :: I | i)
```

#### `legend`

``` purescript
legend :: forall i. DSL LegendI -> DSL (legend :: I | i)
```

#### `xAxis`

``` purescript
xAxis :: forall i. DSL XAxisI -> DSL (xAxis :: I | i)
```

#### `yAxis`

``` purescript
yAxis :: forall i. DSL YAxisI -> DSL (yAxis :: I | i)
```

#### `color`

``` purescript
color :: forall i. Color -> DSL (color :: I | i)
```

#### `colors`

``` purescript
colors :: forall i f. Foldable f => f Color -> DSL (color :: I | i)
```

#### `rgbaColors`

``` purescript
rgbaColors :: forall i f. Foldable f => f Color -> DSL (color :: I | i)
```

#### `rgbaColor`

``` purescript
rgbaColor :: forall i. Color -> DSL (color :: I | i)
```

#### `backgroundColor`

``` purescript
backgroundColor :: forall i. Color -> DSL (backgroundColor :: I | i)
```

#### `visible`

``` purescript
visible :: forall i. Boolean -> DSL (show :: I | i)
```

#### `shown`

``` purescript
shown :: forall i. DSL (show :: I | i)
```

#### `hidden`

``` purescript
hidden :: forall i. DSL (show :: I | i)
```

#### `textStyle`

``` purescript
textStyle :: forall i. DSL TextStyleI -> DSL (textStyle :: I | i)
```

#### `left`

``` purescript
left :: forall i. PixelOrPercent -> DSL (left :: I | i)
```

#### `right`

``` purescript
right :: forall i. PixelOrPercent -> DSL (right :: I | i)
```

#### `top`

``` purescript
top :: forall i. PixelOrPercent -> DSL (top :: I | i)
```

#### `topTop`

``` purescript
topTop :: forall i. DSL (top :: I | i)
```

#### `topMiddle`

``` purescript
topMiddle :: forall i. DSL (top :: I | i)
```

#### `topBottom`

``` purescript
topBottom :: forall i. DSL (top :: I | i)
```

#### `bottom`

``` purescript
bottom :: forall i. PixelOrPercent -> DSL (bottom :: I | i)
```

#### `bottomPx`

``` purescript
bottomPx :: forall i. Int -> DSL (bottom :: I | i)
```

#### `orient`

``` purescript
orient :: forall i. Orient -> DSL (orient :: I | i)
```

#### `items`

``` purescript
items :: forall i f. Foldable f => f Item -> DSL (items :: I | i)
```

#### `itemsDSL`

``` purescript
itemsDSL :: forall i f. Foldable f => f (DSL ItemI) -> DSL (items :: I | i)
```

#### `addItem`

``` purescript
addItem :: forall i. DSL ItemI -> DSL (item :: I | i)
```

#### `buildItems`

``` purescript
buildItems :: forall i. DSL ItemsI -> DSL (items :: I | i)
```

#### `visibleContent`

``` purescript
visibleContent :: forall i. Boolean -> DSL (showContent :: I | i)
```

#### `showContent`

``` purescript
showContent :: forall i. DSL (showContent :: I | i)
```

#### `hideContent`

``` purescript
hideContent :: forall i. DSL (showContent :: I | i)
```

#### `trigger`

``` purescript
trigger :: forall i. TooltipTrigger -> DSL (trigger :: I | i)
```

#### `triggerAxis`

``` purescript
triggerAxis :: forall i. DSL (trigger :: I | i)
```

#### `triggerItem`

``` purescript
triggerItem :: forall i. DSL (trigger :: I | i)
```

#### `pie`

``` purescript
pie :: forall i. DSL PieSeriesI -> DSL (pie :: I | i)
```

#### `line`

``` purescript
line :: forall i. DSL LineSeriesI -> DSL (line :: I | i)
```

#### `bar`

``` purescript
bar :: forall i. DSL BarSeriesI -> DSL (bar :: I | i)
```

#### `scatter`

``` purescript
scatter :: forall i. DSL ScatterI -> DSL (scatter :: I | i)
```

#### `effectScatter`

``` purescript
effectScatter :: forall i. DSL EffectScatterI -> DSL (effectScatter :: I | i)
```

#### `treeMap`

``` purescript
treeMap :: forall i. DSL TreeMapI -> DSL (treeMap :: I | i)
```

#### `boxPlot`

``` purescript
boxPlot :: forall i. DSL BoxPlotI -> DSL (boxPlot :: I | i)
```

#### `candlestick`

``` purescript
candlestick :: forall i. DSL CandlestickI -> DSL (candlestick :: I | i)
```

#### `heatMap`

``` purescript
heatMap :: forall i. DSL HeatMapI -> DSL (heatMap :: I | i)
```

#### `map_`

``` purescript
map_ :: forall i. DSL MapI -> DSL (map :: I | i)
```

#### `parallel`

``` purescript
parallel :: forall i. DSL ParallelI -> DSL (parallel :: I | i)
```

#### `lines`

``` purescript
lines :: forall i. DSL LinesI -> DSL (lines :: I | i)
```

#### `graph`

``` purescript
graph :: forall i. DSL GraphI -> DSL (graph :: I | i)
```

#### `sankey`

``` purescript
sankey :: forall i. DSL SankeyI -> DSL (sankey :: I | i)
```

#### `funnel`

``` purescript
funnel :: forall i. DSL FunnelI -> DSL (funnel :: I | i)
```

#### `gauge`

``` purescript
gauge :: forall i. DSL GaugeI -> DSL (gauge :: I | i)
```

#### `radarSeries`

``` purescript
radarSeries :: forall i. DSL RadarSeriesI -> DSL (radarSeries :: I | i)
```

#### `xAxisIndex`

``` purescript
xAxisIndex :: forall i. Int -> DSL (xAxisIndex :: I | i)
```

#### `yAxisIndex`

``` purescript
yAxisIndex :: forall i. Int -> DSL (yAxisIndex :: I | i)
```

#### `polarIndex`

``` purescript
polarIndex :: forall i. Int -> DSL (polarIndex :: I | i)
```

#### `symbol`

``` purescript
symbol :: forall i. Symbol -> DSL (symbol :: I | i)
```

#### `symbolSize`

``` purescript
symbolSize :: forall i. Int -> DSL (symbolSize :: I | i)
```

#### `lineStyle`

``` purescript
lineStyle :: forall i. DSL LineStyleI -> DSL (lineStyle :: I | i)
```

#### `areaStyle`

``` purescript
areaStyle :: forall i. DSL AreaStyleI -> DSL (areaStyle :: I | i)
```

#### `smooth`

``` purescript
smooth :: forall i. Boolean -> DSL (smooth :: I | i)
```

#### `name`

``` purescript
name :: forall i. String -> DSL (name :: I | i)
```

#### `stack`

``` purescript
stack :: forall i. String -> DSL (stack :: I | i)
```

#### `center`

``` purescript
center :: forall i. Point -> DSL (center :: I | i)
```

#### `radius`

``` purescript
radius :: forall i. Radius -> DSL (radius :: I | i)
```

#### `startAngle`

``` purescript
startAngle :: forall i. Number -> DSL (startAngle :: I | i)
```

#### `axisTick`

``` purescript
axisTick :: forall i. DSL AxisTickI -> DSL (axisTick :: I | i)
```

#### `axisLabel`

``` purescript
axisLabel :: forall i. DSL AxisLabelI -> DSL (axisLabel :: I | i)
```

#### `axisType`

``` purescript
axisType :: forall i. AxisType -> DSL (axisType :: I | i)
```

#### `value`

``` purescript
value :: forall i. Number -> DSL (value :: I | i)
```

#### `values`

``` purescript
values :: forall i f. Foldable f => f Number -> DSL (value :: I | i)
```

#### `valuePair`

``` purescript
valuePair :: forall i. String -> Number -> DSL (value :: I | i)
```

#### `title`

``` purescript
title :: forall i. DSL TitleI -> DSL (title :: I | i)
```

#### `text`

``` purescript
text :: forall i. String -> DSL (text :: I | i)
```

#### `showDelay`

``` purescript
showDelay :: forall i. Number -> DSL (showDelay :: I | i)
```

#### `pointerType`

``` purescript
pointerType :: forall i. PointerType -> DSL (pointerType :: I | i)
```

#### `zlevel`

``` purescript
zlevel :: forall i. Int -> DSL (zlevel :: I | i)
```

#### `lineType`

``` purescript
lineType :: forall i. LineType -> DSL (lineType :: I | i)
```

#### `width`

``` purescript
width :: forall i. Int -> DSL (width :: I | i)
```

#### `widthPct`

``` purescript
widthPct :: forall i. Number -> DSL (width :: I | i)
```

#### `axisPointer`

``` purescript
axisPointer :: forall i. DSL AxisPointerI -> DSL (axisPointer :: I | i)
```

#### `scale`

``` purescript
scale :: forall i. Boolean -> DSL (scale :: I | i)
```

#### `large`

``` purescript
large :: forall i. Boolean -> DSL (large :: I | i)
```

#### `formatterAxis`

``` purescript
formatterAxis :: forall i. (Array FormatterInput -> String) -> DSL (formatter :: I | i)
```

#### `formatterItem`

``` purescript
formatterItem :: forall i. (FormatterInput -> String) -> DSL (formatter :: I | i)
```

#### `formatterString`

``` purescript
formatterString :: forall i. String -> DSL (formatter :: I | i)
```

#### `formatterValue`

``` purescript
formatterValue :: forall i. (Number -> String) -> DSL (formatter :: I | i)
```

#### `animationEnabled`

``` purescript
animationEnabled :: forall i. Boolean -> DSL (animation :: I | i)
```

#### `splitLine`

``` purescript
splitLine :: forall i. DSL SplitLineI -> DSL (splitLine :: I | i)
```

#### `boundaryGap`

``` purescript
boundaryGap :: forall i. Point -> DSL (boundaryGap :: I | i)
```

#### `disabledBoundaryGap`

``` purescript
disabledBoundaryGap :: forall i. DSL (boundaryGap :: I | i)
```

#### `enabledBoundaryGap`

``` purescript
enabledBoundaryGap :: forall i. DSL (boundaryGap :: I | i)
```

#### `hoverAnimationEnabled`

``` purescript
hoverAnimationEnabled :: forall i. Boolean -> DSL (hoverAnimation :: I | i)
```

#### `showSymbol`

``` purescript
showSymbol :: forall i. Boolean -> DSL (showSymbol :: I | i)
```

#### `selectedMode`

``` purescript
selectedMode :: forall i. SelectedMode -> DSL (selectedMode :: I | i)
```

#### `label`

``` purescript
label :: forall i. DSL LabelI -> DSL (label :: I | i)
```

#### `normalLabel`

``` purescript
normalLabel :: forall i. DSL LabelInnerI -> DSL (normalLabel :: I | i)
```

#### `emphasisLabel`

``` purescript
emphasisLabel :: forall i. DSL LabelInnerI -> DSL (emphasisLabel :: I | i)
```

#### `selected`

``` purescript
selected :: forall i. Boolean -> DSL (selected :: I | i)
```

#### `leftPosition`

``` purescript
leftPosition :: forall i. HorizontalPosition -> DSL (left :: I | i)
```

#### `alignLeft`

``` purescript
alignLeft :: forall i. DSL (align :: I | i)
```

#### `alignRight`

``` purescript
alignRight :: forall i. DSL (align :: I | i)
```

#### `alignAuto`

``` purescript
alignAuto :: forall i. DSL (align :: I | i)
```

#### `brush`

``` purescript
brush :: forall i. DSL BrushI -> DSL (brush :: I | i)
```

#### `brushToolbox`

``` purescript
brushToolbox :: forall i. DSL BrushToolboxI -> DSL (brushToolbox :: I | i)
```

#### `rect`

``` purescript
rect :: forall i. DSL (tool :: I | i)
```

#### `polygon`

``` purescript
polygon :: forall i. DSL (tool :: I | i)
```

#### `lineX`

``` purescript
lineX :: forall i. DSL (tool :: I | i)
```

#### `lineY`

``` purescript
lineY :: forall i. DSL (tool :: I | i)
```

#### `keep`

``` purescript
keep :: forall i. DSL (tool :: I | i)
```

#### `clear`

``` purescript
clear :: forall i. DSL (tool :: I | i)
```

#### `toolbox`

``` purescript
toolbox :: forall i. DSL ToolboxI -> DSL (toolbox :: I | i)
```

#### `feature`

``` purescript
feature :: forall i. DSL FeatureI -> DSL (feature :: I | i)
```

#### `magicType`

``` purescript
magicType :: forall i. DSL MagicTypeI -> DSL (magicType :: I | i)
```

#### `magics`

``` purescript
magics :: forall i. DSL MagicsI -> DSL (magics :: I | i)
```

#### `magicLine`

``` purescript
magicLine :: forall i. DSL (magic :: I | i)
```

#### `magicBar`

``` purescript
magicBar :: forall i. DSL (magic :: I | i)
```

#### `magicStack`

``` purescript
magicStack :: forall i. DSL (magic :: I | i)
```

#### `magicTiled`

``` purescript
magicTiled :: forall i. DSL (magic :: I | i)
```

#### `dataView`

``` purescript
dataView :: forall i. DSL DataViewI -> DSL (dataView :: I | i)
```

#### `splitArea`

``` purescript
splitArea :: forall i. DSL SplitAreaI -> DSL (splitArea :: I | i)
```

#### `axisLine`

``` purescript
axisLine :: forall i. DSL AxisLineI -> DSL (axisLine :: I | i)
```

#### `silent`

``` purescript
silent :: forall i. Boolean -> DSL (silent :: I | i)
```

#### `onZero`

``` purescript
onZero :: forall i. Boolean -> DSL (onZero :: I | i)
```

#### `inverse`

``` purescript
inverse :: forall i. Boolean -> DSL (inverse :: I | i)
```

#### `visualMap`

``` purescript
visualMap :: forall i. DSL VisualMapI -> DSL (visualMap :: I | i)
```

#### `continuous`

``` purescript
continuous :: forall i. DSL ContinuousVisualMapI -> DSL (continuousVisualMap :: I | i)
```

#### `dimension`

``` purescript
dimension :: forall i. Int -> DSL (dimension :: I | i)
```

#### `textPair`

``` purescript
textPair :: forall i. String -> String -> DSL (textPair :: I | i)
```

#### `itemHeight`

``` purescript
itemHeight :: forall i. Number -> DSL (itemHeight :: I | i)
```

#### `calculable`

``` purescript
calculable :: forall i. Boolean -> DSL (calculable :: I | i)
```

#### `min`

``` purescript
min :: forall i. Number -> DSL (min :: I | i)
```

#### `max`

``` purescript
max :: forall i. Number -> DSL (max :: I | i)
```

#### `inRange`

``` purescript
inRange :: forall i. DSL InOutRangeI -> DSL (inRange :: I | i)
```

#### `outOfRange`

``` purescript
outOfRange :: forall i. DSL InOutRangeI -> DSL (outOfRange :: I | i)
```

#### `controller`

``` purescript
controller :: forall i. DSL ControllerI -> DSL (controller :: I | i)
```

#### `colorLightness`

``` purescript
colorLightness :: forall i. Number -> Number -> DSL (colorLightness :: I | i)
```

#### `itemStyle`

``` purescript
itemStyle :: forall i. DSL ItemStyleI -> DSL (itemStyle :: I | i)
```

#### `normalItemStyle`

``` purescript
normalItemStyle :: forall i. DSL IStyleI -> DSL (normalItemStyle :: I | i)
```

#### `emphasisItemStyle`

``` purescript
emphasisItemStyle :: forall i. DSL IStyleI -> DSL (emphasisItemStyle :: I | i)
```

#### `barBorderWidth`

``` purescript
barBorderWidth :: forall i. Number -> DSL (barBorderWidth :: I | i)
```

#### `shadowBlur`

``` purescript
shadowBlur :: forall i. Number -> DSL (shadowBlur :: I | i)
```

#### `shadowOffsetX`

``` purescript
shadowOffsetX :: forall i. Number -> DSL (shadowOffsetX :: I | i)
```

#### `shadowOffsetY`

``` purescript
shadowOffsetY :: forall i. Number -> DSL (shadowOffsetY :: I | i)
```

#### `shadowColor`

``` purescript
shadowColor :: forall i. Color -> DSL (shadowColor :: I | i)
```

#### `restore`

``` purescript
restore :: forall i. DSL RestoreI -> DSL (restore :: I | i)
```

#### `saveAsImage`

``` purescript
saveAsImage :: forall i. DSL SaveAsImageI -> DSL (saveAsImage :: I | i)
```

#### `z`

``` purescript
z :: forall i. Int -> DSL (z :: I | i)
```

#### `splitNumber`

``` purescript
splitNumber :: forall i. Int -> DSL (splitNumber :: I | i)
```

#### `gaugeRadius`

``` purescript
gaugeRadius :: forall i. PixelOrPercent -> DSL (gaugeRadius :: I | i)
```

#### `detail`

``` purescript
detail :: forall i. DSL DetailI -> DSL (detail :: I | i)
```

#### `endAngle`

``` purescript
endAngle :: forall i. Number -> DSL (endAngle :: I | i)
```

#### `gaugePointer`

``` purescript
gaugePointer :: forall i. DSL GaugePointerI -> DSL (gaugePointer :: I | i)
```

#### `length`

``` purescript
length :: forall i. Int -> DSL (length :: I | i)
```

#### `autoColor`

``` purescript
autoColor :: forall i. DSL (color :: I | i)
```

#### `bolderFontWeight`

``` purescript
bolderFontWeight :: forall i. DSL (fontWeight :: I | i)
```

#### `fontSize`

``` purescript
fontSize :: forall i. Int -> DSL (fontSize :: I | i)
```

#### `italicFontStyle`

``` purescript
italicFontStyle :: forall i. DSL (fontStyle :: I | i)
```

#### `offsetCenter`

``` purescript
offsetCenter :: forall i. Point -> DSL (offsetCenter :: I | i)
```

#### `subtext`

``` purescript
subtext :: forall i. String -> DSL (subtext :: I | i)
```

#### `readOnly`

``` purescript
readOnly :: forall i. Boolean -> DSL (readOnly :: I | i)
```

#### `positionInside`

``` purescript
positionInside :: forall i. DSL (position :: I | i)
```

#### `labelLine`

``` purescript
labelLine :: forall i. DSL LabelLineI -> DSL (labelLine :: I | i)
```

#### `normalLabelLine`

``` purescript
normalLabelLine :: forall i. DSL LabelLineInnerI -> DSL (normalLabelLine :: I | i)
```

#### `emphasisLabelLine`

``` purescript
emphasisLabelLine :: forall i. DSL LabelLineInnerI -> DSL (emphasisLabelLine :: I | i)
```

#### `opacity`

``` purescript
opacity :: forall i. Number -> DSL (opacity :: I | i)
```

#### `maxSize`

``` purescript
maxSize :: forall i. Int -> DSL (maxSize :: I | i)
```

#### `maxSizePct`

``` purescript
maxSizePct :: forall i. Number -> DSL (maxSize :: I | i)
```

#### `borderColor`

``` purescript
borderColor :: forall i. Color -> DSL (borderColor :: I | i)
```

#### `borderWidth`

``` purescript
borderWidth :: forall i. Int -> DSL (borderWidth :: I | i)
```

#### `normalLineStyle`

``` purescript
normalLineStyle :: forall i. DSL LineStyleI -> DSL (normalLineStyle :: I | i)
```

#### `emphasisLineStyle`

``` purescript
emphasisLineStyle :: forall i. DSL LineStyleI -> DSL (emphasisLineStyle :: I | i)
```

#### `leftCenter`

``` purescript
leftCenter :: forall i. DSL (left :: I | i)
```

#### `leftLeft`

``` purescript
leftLeft :: forall i. DSL (left :: I | i)
```

#### `leftRight`

``` purescript
leftRight :: forall i. DSL (left :: I | i)
```

#### `itemGap`

``` purescript
itemGap :: forall i. Int -> DSL (itemGap :: I | i)
```

#### `indicators`

``` purescript
indicators :: forall i. DSL IndicatorsI -> DSL (indicators :: I | i)
```

#### `indicator`

``` purescript
indicator :: forall i. DSL IndicatorI -> DSL (indicator :: I | i)
```

#### `radarName`

``` purescript
radarName :: forall i. DSL RadarNameI -> DSL (radarName :: I | i)
```

#### `polygonShape`

``` purescript
polygonShape :: forall i. DSL (shape :: I | i)
```

#### `circleShape`

``` purescript
circleShape :: forall i. DSL (shape :: I | i)
```

#### `lineStylePair`

``` purescript
lineStylePair :: forall i. DSL LineStylePairI -> DSL (lineStylePair :: I | i)
```

#### `areaStylePair`

``` purescript
areaStylePair :: forall i. DSL AreaStylePairI -> DSL (areaStylePair :: I | i)
```

#### `normalAreaStyle`

``` purescript
normalAreaStyle :: forall i. DSL AreaStyleI -> DSL (normalAreaStyle :: I | i)
```

#### `emphasisAreaStyle`

``` purescript
emphasisAreaStyle :: forall i. DSL AreaStyleI -> DSL (emphasisAreaStyle :: I | i)
```

#### `radar`

``` purescript
radar :: forall i. DSL RadarI -> DSL (radar :: I | i)
```

#### `ascending`

``` purescript
ascending :: forall i. DSL (sort :: I | i)
```

#### `descending`

``` purescript
descending :: forall i. DSL (sort :: I | i)
```

#### `animationDurationUpdate`

``` purescript
animationDurationUpdate :: forall i. Int -> DSL (animationDurationUpdate :: I | i)
```

#### `animationEasingUpdateQuinticInOut`

``` purescript
animationEasingUpdateQuinticInOut :: forall i. DSL (animationEasingUpdate :: I | i)
```

#### `roam`

``` purescript
roam :: forall i. Boolean -> DSL (roam :: I | i)
```

#### `edgeSymbols`

``` purescript
edgeSymbols :: forall i. DSL EdgeSymbolsI -> DSL (edgeSymbols :: I | i)
```

#### `circleEdgeSymbol`

``` purescript
circleEdgeSymbol :: forall i. DSL (edgeSymbol :: I | i)
```

#### `arrowEdgeSymbol`

``` purescript
arrowEdgeSymbol :: forall i. DSL (edgeSymbol :: I | i)
```

#### `edgeSymbolSize`

``` purescript
edgeSymbolSize :: forall i. Int -> DSL (edgeSymbolSize :: I | i)
```

#### `edgeSymbolSizes`

``` purescript
edgeSymbolSizes :: forall i. Int -> Int -> DSL (edgeSymbolSize :: I | i)
```

#### `buildLinks`

``` purescript
buildLinks :: forall i. DSL LinksI -> DSL (links :: I | i)
```

#### `addLink`

``` purescript
addLink :: forall i. DSL LinkI -> DSL (link :: I | i)
```

#### `edgeLabel`

``` purescript
edgeLabel :: forall i. DSL EdgeLabelI -> DSL (edgeLabel :: I | i)
```

#### `normalEdgeLabel`

``` purescript
normalEdgeLabel :: forall i. DSL EdgeLabelInnerI -> DSL (normalEdgeLabel :: I | i)
```

#### `emphasisEdgeLabel`

``` purescript
emphasisEdgeLabel :: forall i. DSL EdgeLabelInnerI -> DSL (emphasisEdgeLabel :: I | i)
```

#### `x`

``` purescript
x :: forall i. Number -> DSL (x :: I | i)
```

#### `y`

``` purescript
y :: forall i. Number -> DSL (y :: I | i)
```

#### `curveness`

``` purescript
curveness :: forall i. Number -> DSL (curveness :: I | i)
```

#### `symbolSizes`

``` purescript
symbolSizes :: forall i. Int -> Int -> DSL (symbolSize :: I | i)
```

#### `symbolSizeArrFunc`

``` purescript
symbolSizeArrFunc :: forall i. (Array Number -> Number) -> DSL (symbolSize :: I | i)
```

#### `sourceIx`

``` purescript
sourceIx :: forall i. Int -> DSL (source :: I | i)
```

#### `targetIx`

``` purescript
targetIx :: forall i. Int -> DSL (target :: I | i)
```

#### `sourceName`

``` purescript
sourceName :: forall i. String -> DSL (source :: I | i)
```

#### `targetName`

``` purescript
targetName :: forall i. String -> DSL (target :: I | i)
```

#### `layoutNone`

``` purescript
layoutNone :: forall i. DSL (layout :: I | i)
```

#### `missingSeries`

``` purescript
missingSeries :: forall i. DSL (missing :: I | i)
```

#### `missingItem`

``` purescript
missingItem :: forall i. DSL (item :: I | i)
```

#### `rotate`

``` purescript
rotate :: forall i. Number -> DSL (rotate :: I | i)
```

#### `fontFamily`

``` purescript
fontFamily :: forall i. String -> DSL (fontFamily :: I | i)
```

#### `yAxes`

``` purescript
yAxes :: forall i. DSL YAxesI -> DSL (yAxis :: I | i)
```

#### `xAxes`

``` purescript
xAxes :: forall i. DSL XAxesI -> DSL (xAxis :: I | i)
```

#### `addYAxis`

``` purescript
addYAxis :: forall i. DSL YAxisI -> DSL (addYAxis :: I | i)
```

#### `addXAxis`

``` purescript
addXAxis :: forall i. DSL XAxisI -> DSL (addXAxis :: I | i)
```

#### `interval`

``` purescript
interval :: forall i. Int -> DSL (interval :: I | i)
```

#### `lineAxisPointer`

``` purescript
lineAxisPointer :: forall i. DSL (axisPointerType :: I | i)
```

#### `crossAxisPointer`

``` purescript
crossAxisPointer :: forall i. DSL (axisPointerType :: I | i)
```

#### `solidLine`

``` purescript
solidLine :: forall i. DSL (lineType :: I | i)
```

#### `dashedLine`

``` purescript
dashedLine :: forall i. DSL (lineType :: I | i)
```

#### `dottedLine`

``` purescript
dottedLine :: forall i. DSL (lineType :: I | i)
```

#### `widthNum`

``` purescript
widthNum :: forall i. Number -> DSL (width :: I | i)
```

#### `crossStyle`

``` purescript
crossStyle :: forall i. DSL CrossStyleI -> DSL (crossStyle :: I | i)
```


