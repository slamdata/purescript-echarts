module ECharts.Commands where

import Prelude

import Color as C

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (toForeign)

import ECharts.Monad (DSL, set, buildObj, buildSeries, buildArr)
import ECharts.Types as T
import ECharts.Types.Phantom (I)
import ECharts.Types.Phantom as TP

buildOption ∷ DSL TP.OptionI Unit → T.Option
buildOption = T.Option <<< buildObj

seriesF ∷ ∀ i. T.Series → DSL (series ∷ I|i) Unit
seriesF a = set "series" $ toForeign a

series ∷ ∀ i. DSL TP.SeriesI Unit → DSL (series ∷ I|i) Unit
series = seriesF <<< T.Series <<< buildSeries

tooltipF ∷ ∀ i. T.Tooltip → DSL (tooltip ∷ I|i) Unit
tooltipF a = set "tooltip" $ toForeign a

tooltip ∷ ∀ i. DSL TP.TooltipI Unit → DSL (tooltip ∷ I|i) Unit
tooltip = tooltipF <<< T.Tooltip <<< buildObj

gridF ∷ ∀ i. T.Grid → DSL (grid ∷ I|i) Unit
gridF a = set "grid" $ toForeign a

grid ∷ ∀ i. DSL TP.GridI Unit → DSL (grid ∷ I|i) Unit
grid = gridF <<< T.Grid <<< buildObj

legendF ∷ ∀ i. T.Legend → DSL (legend ∷ I|i) Unit
legendF a = set "legend" $ toForeign a

legend ∷ ∀ i. DSL TP.LegendI Unit → DSL (legend ∷ I|i) Unit
legend = legendF <<< T.Legend <<< buildObj

xAxisF ∷ ∀ i. T.XAxis → DSL (xAxis ∷ I|i) Unit
xAxisF a = set "xAxis" $ toForeign a

xAxis ∷ ∀ i. DSL TP.XAxisI Unit → DSL (xAxis ∷ I|i) Unit
xAxis = xAxisF <<< T.XAxis <<< buildObj

yAxisF ∷ ∀ i. T.YAxis → DSL (yAxis ∷ I|i) Unit
yAxisF a = set "yAxis" $ toForeign a

yAxis ∷ ∀ i. DSL TP.YAxisI Unit → DSL (yAxis ∷ I|i) Unit
yAxis = yAxisF <<< T.YAxis <<< buildObj

color ∷ ∀ i f. F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i) Unit
color colors = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.toHexString) colors

visible ∷ ∀ i. Boolean → DSL (show ∷ I|i) Unit
visible a = set "show" $ toForeign a

shown ∷ ∀ i. DSL (show ∷ I|i) Unit
shown = visible true

hidden ∷ ∀ i. DSL (show ∷ I|i) Unit
hidden = visible false

textStyle ∷ ∀ i. T.TextStyle → DSL (textStyle ∷ I|i) Unit
textStyle a = set "textStyle" $ toForeign a

left ∷ ∀ i. T.PixelOrPercent → DSL (left ∷ I|i) Unit
left a = set "left" $ T.pixelOrPercentToForeign a

right ∷ ∀ i. T.PixelOrPercent → DSL (right ∷ I|i) Unit
right a = set "right" $ T.pixelOrPercentToForeign a

top ∷ ∀ i. T.PixelOrPercent → DSL (top ∷ I|i) Unit
top a = set "top" $ T.pixelOrPercentToForeign a

bottom ∷ ∀ i. T.PixelOrPercent → DSL (bottom ∷ I|i) Unit
bottom a = set "bottom" $ T.pixelOrPercentToForeign a

orient ∷ ∀ i. T.Orient → DSL (orient ∷ I|i) Unit
orient a = set "orient" $ T.orientToForeign a

items ∷ ∀ i f. F.Foldable f ⇒ f T.Item → DSL (items ∷ I|i) Unit
items a = set "data" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

itemsDSL ∷ ∀ i f. F.Foldable f ⇒ f (DSL TP.ItemI Unit) → DSL (items ∷ I|i) Unit
itemsDSL a = set "data" $ toForeign $ F.foldMap (Arr.singleton <<< T.Item <<< buildObj) a

addItemF ∷ ∀ i. T.Item → DSL (item ∷ I|i) Unit
addItemF a = set "" $ toForeign a

addItem ∷ ∀ i. DSL TP.ItemI Unit → DSL (item ∷ I|i) Unit
addItem = addItemF <<< T.Item <<< buildObj

buildItems ∷ ∀ i. DSL TP.ItemsI Unit → DSL (items ∷ I|i) Unit
buildItems is = set "data" $ buildArr is

visibleContent ∷ ∀ i. Boolean → DSL (showContent ∷ I|i) Unit
visibleContent a = set "showContent" $ toForeign a

showContent ∷ ∀ i. DSL (showContent ∷ I|i) Unit
showContent = visibleContent true

hideContent ∷ ∀ i. DSL (showContent ∷ I|i) Unit
hideContent = visibleContent false

trigger ∷ ∀ i. T.TooltipTrigger → DSL (trigger ∷ I|i) Unit
trigger a = set "trigger" $ T.tooltipTriggerToForeign a

pieF ∷ ∀ i. T.PieSeries → DSL (pie ∷ I|i) Unit
pieF a = set "pie" $ toForeign a

lineF ∷ ∀ i. T.LineSeries → DSL (line ∷ I|i) Unit
lineF a = set "line" $ toForeign a

barF ∷ ∀ i. T.BarSeries → DSL (bar ∷ I|i) Unit
barF a = set "bar" $ toForeign a

scatterF ∷ ∀ i. T.ScatterSeries → DSL (scatter ∷ I|i) Unit
scatterF a = set "scatter" $ toForeign a

effectScatterF ∷ ∀ i. T.EffectScatterSeries → DSL (effectScatter ∷ I|i) Unit
effectScatterF a = set "effectScatter" $ toForeign a

radarF ∷ ∀ i. T.RadarSeries → DSL (radar ∷ I|i) Unit
radarF a = set "radar" $ toForeign a

treeMapF ∷ ∀ i. T.TreeMapSeries → DSL (treeMap ∷ I|i) Unit
treeMapF a = set "treemap" $ toForeign a

boxPlotF ∷ ∀ i. T.BoxPlotSeries → DSL (boxPlot ∷ I|i) Unit
boxPlotF a = set "boxplot" $ toForeign a

candlestickF ∷ ∀ i. T.CandlestickSeries → DSL (candlestick ∷ I|i) Unit
candlestickF a = set "candlestick" $ toForeign a

heatMapF ∷ ∀ i. T.HeatMapSeries → DSL (heatMap ∷ I|i) Unit
heatMapF a = set "heatmap" $ toForeign a

mapF ∷ ∀ i. T.MapSeries → DSL (map ∷ I|i) Unit
mapF a = set "map" $ toForeign a

parallelF ∷ ∀ i. T.ParallelSeries → DSL (parallel ∷ I|i) Unit
parallelF a = set "parallel" $ toForeign a

linesF ∷ ∀ i. T.LinesSeries → DSL (lines ∷ I|i) Unit
linesF a = set "lines" $ toForeign a

graphF ∷ ∀ i. T.GraphSeries → DSL (graph ∷ I|i) Unit
graphF a = set "graph" $ toForeign a

sankeyF ∷ ∀ i. T.SankeySeries → DSL (sankey ∷ I|i) Unit
sankeyF a = set "sankey" $ toForeign a

funnelF ∷ ∀ i. T.FunnelSeries → DSL (funnel ∷ I|i) Unit
funnelF a = set "funnel" $ toForeign a

gaugeF ∷ ∀ i. T.GaugeSeries → DSL (gauge ∷ I|i) Unit
gaugeF a = set "gauge" $ toForeign a

pie ∷ ∀ i. DSL TP.PieSeriesI Unit → DSL (pie ∷ I|i) Unit
pie = pieF <<< T.PieSeries <<< buildObj

line ∷ ∀ i. DSL TP.LineSeriesI Unit → DSL (line ∷ I|i) Unit
line = lineF <<< T.LineSeries <<< buildObj

bar ∷ ∀ i. DSL TP.BarSeriesI Unit → DSL (bar ∷ I|i) Unit
bar = barF <<< T.BarSeries <<< buildObj

scatter ∷ ∀ i. DSL TP.ScatterI Unit → DSL (scatter ∷ I|i) Unit
scatter = scatterF <<< T.ScatterSeries <<< buildObj

effectScatter ∷ ∀ i. DSL TP.EffectScatterI Unit → DSL (effectScatter ∷ I|i) Unit
effectScatter = effectScatterF <<< T.EffectScatterSeries <<< buildObj

treeMap ∷ ∀ i. DSL TP.TreeMapI Unit → DSL (treeMap ∷ I|i) Unit
treeMap = treeMapF <<< T.TreeMapSeries <<< buildObj

boxPlot ∷ ∀ i. DSL TP.BoxPlotI Unit → DSL (boxPlot ∷ I|i) Unit
boxPlot = boxPlotF <<< T.BoxPlotSeries <<< buildObj

candlestick ∷ ∀ i. DSL TP.CandlestickI Unit → DSL (candlestick ∷ I|i) Unit
candlestick = candlestickF <<< T.CandlestickSeries <<< buildObj

heatMap ∷ ∀ i. DSL TP.HeatMapI Unit → DSL (heatMap ∷ I|i) Unit
heatMap = heatMapF <<< T.HeatMapSeries <<< buildObj

map_ ∷ ∀ i. DSL TP.MapI Unit → DSL (map ∷ I|i) Unit
map_ = mapF <<< T.MapSeries <<< buildObj

parallel ∷ ∀ i. DSL TP.ParallelI Unit → DSL (parallel ∷ I|i) Unit
parallel = parallelF <<< T.ParallelSeries <<< buildObj

lines ∷ ∀ i. DSL TP.LinesI Unit → DSL (lines ∷ I|i) Unit
lines = linesF <<< T.LinesSeries <<< buildObj

graph ∷ ∀ i. DSL TP.GraphI Unit → DSL (graph ∷ I|i) Unit
graph = graphF <<< T.GraphSeries <<< buildObj

sankey ∷ ∀ i. DSL TP.SankeyI Unit → DSL (sankey ∷ I|i) Unit
sankey = sankeyF <<< T.SankeySeries <<< buildObj

funnel ∷ ∀ i. DSL TP.FunnelI Unit → DSL (funnel ∷ I|i) Unit
funnel = funnelF <<< T.FunnelSeries <<< buildObj

gauge ∷ ∀ i. DSL TP.GaugeI Unit → DSL (gauge ∷ I|i) Unit
gauge = gaugeF <<< T.GaugeSeries <<< buildObj

xAxisIndex ∷ ∀ i. Int → DSL (xAxisIndex ∷ I|i) Unit
xAxisIndex a = set "xAxisIndex" $ toForeign a

yAxisIndex ∷ ∀ i. Int → DSL (yAxisIndex ∷ I|i) Unit
yAxisIndex a = set "yAxisIndex" $ toForeign a

polarIndex ∷ ∀ i. Int → DSL (polarIndex ∷ I|i) Unit
polarIndex a = set "polarIndex" $ toForeign a

symbol ∷ ∀ i. T.Symbol → DSL (symbol ∷ I|i) Unit
symbol a = set "symbol" $ T.symbolToForeign a

symbolSizeF ∷ ∀ i. T.SymbolSize → DSL (symbolSize ∷ I|i) Unit
symbolSizeF a = set "symbolSize" $ toForeign a

lineStyleF ∷ ∀ i. T.LineStyle → DSL (lineStyle ∷ I|i) Unit
lineStyleF a = set "lineStyle" $ toForeign a

lineStyle ∷ ∀ i. DSL TP.LineStyleI Unit → DSL (lineStyle ∷ I|i) Unit
lineStyle = lineStyleF <<< T.LineStyle <<< buildObj

areaStyleF ∷ ∀ i. T.AreaStyle → DSL (areaStyle ∷ I|i) Unit
areaStyleF a = set "areaStyle" $ toForeign a

smooth ∷ ∀ i. Boolean → DSL (smooth ∷ I|i) Unit
smooth a = set "smooth" $ toForeign a

name ∷ ∀ i. String → DSL (name ∷ I|i) Unit
name a = set "name" $ toForeign a

stack ∷ ∀ i. String → DSL (stack ∷ I|i) Unit
stack a = set "stack" $ toForeign a

center ∷ ∀ i. T.Point → DSL (center ∷ I|i) Unit
center a = set "center" $ T.pointToForeign a

radius ∷ ∀ i. T.Radius → DSL (radius ∷ I|i) Unit
radius a = set "radius" $ T.radiusToForeign a

startAngle ∷ ∀ i. Number → DSL (startAngle ∷ I|i) Unit
startAngle a = set "startAngle" $ toForeign a

axisTickF ∷ ∀ i. T.AxisTick → DSL (axisTick ∷ I|i) Unit
axisTickF a = set "axisTick" $ toForeign a

axisLabelF ∷ ∀ i. T.AxisLabel → DSL (axisLabel ∷ I|i) Unit
axisLabelF a = set "axisLabel" $ toForeign a

axisType ∷ ∀ i. T.AxisType → DSL (axisType ∷ I|i) Unit
axisType a = set "type" $ T.axisTypeToForeign a

value ∷ ∀ i. Number → DSL (value ∷ I|i) Unit
value a = set "value" $ toForeign a

valuePair ∷ ∀ i. String → Number → DSL (value ∷ I|i) Unit
valuePair a b = set "value" $ toForeign [toForeign a, toForeign b]

titleF ∷ ∀ i. T.Title → DSL (title ∷ I|i) Unit
titleF a = set "title" $ toForeign a

title ∷ ∀ i. DSL TP.TitleI Unit → DSL (title ∷ I|i) Unit
title = titleF <<< T.Title <<< buildObj

text ∷ ∀ i. String → DSL (text ∷ I|i) Unit
text a = set "text" $ toForeign a

showDelay ∷ ∀ i. Number → DSL (showDelay ∷ I|i) Unit
showDelay a = set "showDelay" $ toForeign a

pointerType ∷ ∀ i. T.PointerType → DSL (pointerType ∷ I|i) Unit
pointerType a = set "type" $ T.pointerTypeToForeign a

zlevel ∷ ∀ i. Int → DSL (zlevel ∷ I|i) Unit
zlevel a = set "zlevel" $ toForeign a

lineType ∷ ∀ i. T.LineType → DSL (lineType ∷ I|i) Unit
lineType a = set "type" $ toForeign a

width ∷ ∀ i. Int → DSL (width ∷ I|i) Unit
width a = set "width" $ toForeign a

axisPointerF ∷ ∀ i. T.AxisPointer → DSL (axisPointer ∷ I|i) Unit
axisPointerF a = set "axisPointer" $ toForeign a

axisPointer ∷ ∀ i. DSL TP.AxisPointerI Unit → DSL (axisPointer ∷ I|i) Unit
axisPointer = axisPointerF <<< T.AxisPointer <<< buildObj

scale ∷ ∀ i. Boolean → DSL (scale ∷ I|i) Unit
scale a = set "scale" $ toForeign a

large ∷ ∀ i. Boolean → DSL (large ∷ I|i) Unit
large a = set "large" $ toForeign a

formatterAxis ∷ ∀ i. (Array T.FormatterInput → String) → DSL (formatter ∷ I|i) Unit
formatterAxis a = set "formatter" $ toForeign a

formatterItem ∷ ∀ i. (T.FormatterInput → String) → DSL (formatter ∷ I|i) Unit
formatterItem a = set "formatter" $ toForeign a

formatterString ∷ ∀ i. String → DSL (formatter ∷ I|i) Unit
formatterString a = set "formatter" $ toForeign a

animationEnabled ∷ ∀ i. Boolean → DSL (animation ∷ I|i) Unit
animationEnabled a = set "animation" $ toForeign a

splitLineF ∷ ∀ i. T.SplitLine → DSL (splitLine ∷ I|i) Unit
splitLineF a = set "splitLine" $ toForeign a

splitLine ∷ ∀ i. DSL TP.SplitLineI Unit → DSL (splitLine ∷ I|i) Unit
splitLine = splitLineF <<< T.SplitLine <<< buildObj

boundaryGap ∷ ∀ i. T.Point → DSL (boundaryGap ∷ I|i) Unit
boundaryGap a = set "boundaryGap" $ T.pointToForeign a

hoverAnimationEnabled ∷ ∀ i. Boolean → DSL (hoverAnimation ∷ I|i) Unit
hoverAnimationEnabled a = set "hoverAnimation" $ toForeign a

showSymbol ∷ ∀ i. Boolean → DSL (showSymbol ∷ I|i) Unit
showSymbol a = set "showSymbol" $ toForeign a

selectedMode ∷ ∀ i. T.SelectedMode → DSL (selectedMode ∷ I|i) Unit
selectedMode a = set "selectedMode" $ T.selectedModeToForeign a

labelF ∷ ∀ i. T.Label → DSL (label ∷ I|i) Unit
labelF a = set "label" $ toForeign a

label ∷ ∀ i. DSL TP.LabelI Unit → DSL (label ∷ I|i) Unit
label = labelF <<< T.Label <<< buildObj

normalLabelF ∷ ∀ i. T.LabelInner → DSL (normalLabel ∷ I|i) Unit
normalLabelF a = set "normal" $ toForeign a

normalLabel ∷ ∀ i. DSL TP.LabelInnerI Unit → DSL (normalLabel ∷ I|i) Unit
normalLabel = normalLabelF <<< T.LabelInner <<< buildObj

emphasisLabelF ∷ ∀ i. T.LabelInner → DSL (emphasisLabel ∷ I|i) Unit
emphasisLabelF a = set "emphasis" $ toForeign a

emphasisLabel ∷ ∀ i. DSL TP.LabelInnerI Unit → DSL (emphasisLabel ∷ I|i) Unit
emphasisLabel = emphasisLabelF <<< T.LabelInner <<< buildObj

selected ∷ ∀ i. Boolean → DSL (selected ∷ I|i) Unit
selected a = set "selected" $ toForeign a

leftPosition ∷ ∀ i. T.HorizontalPosition → DSL (left ∷ I|i) Unit
leftPosition a = set "left" $ T.horizontalPositionToForeign a
