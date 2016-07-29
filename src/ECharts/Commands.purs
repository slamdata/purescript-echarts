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

series ∷ ∀ i. DSL TP.SeriesI → DSL (series ∷ I|i)
series a = set "series" $ buildSeries a

tooltip ∷ ∀ i. DSL TP.TooltipI → DSL (tooltip ∷ I|i)
tooltip a = set "tooltip" $ buildObj a

grid ∷ ∀ i. DSL TP.GridI → DSL (grid ∷ I|i)
grid a = set "grid" $ buildObj a

legend ∷ ∀ i. DSL TP.LegendI → DSL (legend ∷ I|i)
legend a = set "legend" $ buildObj a

xAxis ∷ ∀ i. DSL TP.XAxisI → DSL (xAxis ∷ I|i)
xAxis a = set "xAxis" $ buildObj a

yAxis ∷ ∀ i. DSL TP.YAxisI → DSL (yAxis ∷ I|i)
yAxis a = set "yAxis" $ buildObj a

color ∷ ∀ i. C.Color → DSL (color ∷ I|i)
color a = set "color" $ toForeign $ C.cssStringRGBA a

colors ∷ ∀ i f. F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i)
colors a = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.toHexString) a

backgroundColor ∷ ∀ i. C.Color → DSL (backgroundColor ∷ I|i)
backgroundColor a = set "backgroundColor" $ toForeign $ C.toHexString a

visible ∷ ∀ i. Boolean → DSL (show ∷ I|i)
visible a = set "show" $ toForeign a

shown ∷ ∀ i. DSL (show ∷ I|i)
shown = visible true

hidden ∷ ∀ i. DSL (show ∷ I|i)
hidden = visible false

textStyle ∷ ∀ i. DSL TP.TextStyleI → DSL (textStyle ∷ I|i)
textStyle a = set "textStyle" $ buildObj a

left ∷ ∀ i. T.PixelOrPercent → DSL (left ∷ I|i)
left a = set "left" $ T.pixelOrPercentToForeign a

right ∷ ∀ i. T.PixelOrPercent → DSL (right ∷ I|i)
right a = set "right" $ T.pixelOrPercentToForeign a

top ∷ ∀ i. T.PixelOrPercent → DSL (top ∷ I|i)
top a = set "top" $ T.pixelOrPercentToForeign a

bottom ∷ ∀ i. T.PixelOrPercent → DSL (bottom ∷ I|i)
bottom a = set "bottom" $ T.pixelOrPercentToForeign a

orient ∷ ∀ i. T.Orient → DSL (orient ∷ I|i)
orient a = set "orient" $ T.orientToForeign a

items ∷ ∀ i f. F.Foldable f ⇒ f T.Item → DSL (items ∷ I|i)
items a = set "data" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

itemsDSL ∷ ∀ i f. F.Foldable f ⇒ f (DSL TP.ItemI) → DSL (items ∷ I|i)
itemsDSL a = set "data" $ toForeign $ F.foldMap (Arr.singleton <<< T.Item <<< buildObj) a

addItem ∷ ∀ i. DSL TP.ItemI → DSL (item ∷ I|i)
addItem = set "" <<< buildObj

buildItems ∷ ∀ i. DSL TP.ItemsI → DSL (items ∷ I|i)
buildItems is = set "data" $ buildArr is

visibleContent ∷ ∀ i. Boolean → DSL (showContent ∷ I|i)
visibleContent a = set "showContent" $ toForeign a

showContent ∷ ∀ i. DSL (showContent ∷ I|i)
showContent = visibleContent true

hideContent ∷ ∀ i. DSL (showContent ∷ I|i)
hideContent = visibleContent false

trigger ∷ ∀ i. T.TooltipTrigger → DSL (trigger ∷ I|i)
trigger a = set "trigger" $ T.tooltipTriggerToForeign a

triggerAxis ∷ ∀ i. DSL (trigger ∷ I|i)
triggerAxis = set "trigger" $ toForeign "axis"

triggerItem ∷ ∀ i. DSL (trigger ∷ I|i)
triggerItem = set "trigger" $ toForeign "item"

pie ∷ ∀ i. DSL TP.PieSeriesI → DSL (pie ∷ I|i)
pie = set "pie" <<< buildObj

line ∷ ∀ i. DSL TP.LineSeriesI → DSL (line ∷ I|i)
line = set "line" <<< buildObj

bar ∷ ∀ i. DSL TP.BarSeriesI → DSL (bar ∷ I|i)
bar = set "bar" <<< buildObj

scatter ∷ ∀ i. DSL TP.ScatterI → DSL (scatter ∷ I|i)
scatter = set "scatter" <<< buildObj

effectScatter ∷ ∀ i. DSL TP.EffectScatterI → DSL (effectScatter ∷ I|i)
effectScatter = set "effectScatter" <<< buildObj

treeMap ∷ ∀ i. DSL TP.TreeMapI → DSL (treeMap ∷ I|i)
treeMap = set "treemap" <<< buildObj

boxPlot ∷ ∀ i. DSL TP.BoxPlotI → DSL (boxPlot ∷ I|i)
boxPlot = set "boxplot" <<< buildObj

candlestick ∷ ∀ i. DSL TP.CandlestickI → DSL (candlestick ∷ I|i)
candlestick = set "candlestick" <<< buildObj

heatMap ∷ ∀ i. DSL TP.HeatMapI → DSL (heatMap ∷ I|i)
heatMap = set "heatmap" <<< buildObj

map_ ∷ ∀ i. DSL TP.MapI → DSL (map ∷ I|i)
map_ = set "map" <<< buildObj

parallel ∷ ∀ i. DSL TP.ParallelI → DSL (parallel ∷ I|i)
parallel = set "parallel" <<< buildObj

lines ∷ ∀ i. DSL TP.LinesI → DSL (lines ∷ I|i)
lines = set "lines" <<< buildObj

graph ∷ ∀ i. DSL TP.GraphI → DSL (graph ∷ I|i)
graph = set "graph" <<< buildObj

sankey ∷ ∀ i. DSL TP.SankeyI → DSL (sankey ∷ I|i)
sankey = set "sankey" <<< buildObj

funnel ∷ ∀ i. DSL TP.FunnelI → DSL (funnel ∷ I|i)
funnel = set "funnel" <<< buildObj

gauge ∷ ∀ i. DSL TP.GaugeI → DSL (gauge ∷ I|i)
gauge = set "gauge" <<< buildObj

xAxisIndex ∷ ∀ i. Int → DSL (xAxisIndex ∷ I|i)
xAxisIndex a = set "xAxisIndex" $ toForeign a

yAxisIndex ∷ ∀ i. Int → DSL (yAxisIndex ∷ I|i)
yAxisIndex a = set "yAxisIndex" $ toForeign a

polarIndex ∷ ∀ i. Int → DSL (polarIndex ∷ I|i)
polarIndex a = set "polarIndex" $ toForeign a

symbol ∷ ∀ i. T.Symbol → DSL (symbol ∷ I|i)
symbol a = set "symbol" $ T.symbolToForeign a

symbolSize ∷ ∀ i. T.SymbolSize → DSL (symbolSize ∷ I|i)
symbolSize a = set "symbolSize" $ toForeign a

lineStyle ∷ ∀ i. DSL TP.LineStyleI → DSL (lineStyle ∷ I|i)
lineStyle = set "lineStyle" <<< buildObj

areaStyle ∷ ∀ i. DSL TP.AreaStyleI → DSL (areaStyle ∷ I|i)
areaStyle = set "areaStyle" <<< buildObj

smooth ∷ ∀ i. Boolean → DSL (smooth ∷ I|i)
smooth a = set "smooth" $ toForeign a

name ∷ ∀ i. String → DSL (name ∷ I|i)
name a = set "name" $ toForeign a

stack ∷ ∀ i. String → DSL (stack ∷ I|i)
stack a = set "stack" $ toForeign a

center ∷ ∀ i. T.Point → DSL (center ∷ I|i)
center a = set "center" $ T.pointToForeign a

radius ∷ ∀ i. T.Radius → DSL (radius ∷ I|i)
radius a = set "radius" $ T.radiusToForeign a

startAngle ∷ ∀ i. Number → DSL (startAngle ∷ I|i)
startAngle a = set "startAngle" $ toForeign a

axisTick ∷ ∀ i. DSL TP.AxisTickI → DSL (axisTick ∷ I|i)
axisTick = set "axisTick" <<< buildObj

axisLabel ∷ ∀ i. DSL TP.AxisLabelI → DSL (axisLabel ∷ I|i)
axisLabel = set "axisLabel" <<< buildObj

axisType ∷ ∀ i. T.AxisType → DSL (axisType ∷ I|i)
axisType a = set "type" $ T.axisTypeToForeign a

value ∷ ∀ i. Number → DSL (value ∷ I|i)
value a = set "value" $ toForeign a

valuePair ∷ ∀ i. String → Number → DSL (value ∷ I|i)
valuePair a b = set "value" $ toForeign [toForeign a, toForeign b]

title ∷ ∀ i. DSL TP.TitleI → DSL (title ∷ I|i)
title = set "title" <<< buildObj

text ∷ ∀ i. String → DSL (text ∷ I|i)
text a = set "text" $ toForeign a

showDelay ∷ ∀ i. Number → DSL (showDelay ∷ I|i)
showDelay a = set "showDelay" $ toForeign a

pointerType ∷ ∀ i. T.PointerType → DSL (pointerType ∷ I|i)
pointerType a = set "type" $ T.pointerTypeToForeign a

zlevel ∷ ∀ i. Int → DSL (zlevel ∷ I|i)
zlevel a = set "zlevel" $ toForeign a

lineType ∷ ∀ i. T.LineType → DSL (lineType ∷ I|i)
lineType a = set "type" $ toForeign a

width ∷ ∀ i. Int → DSL (width ∷ I|i)
width a = set "width" $ toForeign a

widthPct ∷ ∀ i. Number → DSL (width ∷ I|i)
widthPct = set "width" <<< toForeign <<< (_ <> "%") <<< show

axisPointer ∷ ∀ i. DSL TP.AxisPointerI → DSL (axisPointer ∷ I|i)
axisPointer = set "axisPointer" <<< buildObj

scale ∷ ∀ i. Boolean → DSL (scale ∷ I|i)
scale a = set "scale" $ toForeign a

large ∷ ∀ i. Boolean → DSL (large ∷ I|i)
large a = set "large" $ toForeign a

formatterAxis ∷ ∀ i. (Array T.FormatterInput → String) → DSL (formatter ∷ I|i)
formatterAxis a = set "formatter" $ toForeign a

formatterItem ∷ ∀ i. (T.FormatterInput → String) → DSL (formatter ∷ I|i)
formatterItem a = set "formatter" $ toForeign a

formatterString ∷ ∀ i. String → DSL (formatter ∷ I|i)
formatterString a = set "formatter" $ toForeign a

formatterValue ∷ ∀ i. (Number → String) → DSL (formatter ∷ I|i)
formatterValue = set "formatter" <<< toForeign

animationEnabled ∷ ∀ i. Boolean → DSL (animation ∷ I|i)
animationEnabled a = set "animation" $ toForeign a

splitLine ∷ ∀ i. DSL TP.SplitLineI → DSL (splitLine ∷ I|i)
splitLine = set "splitLine" <<< buildObj

boundaryGap ∷ ∀ i. T.Point → DSL (boundaryGap ∷ I|i)
boundaryGap a = set "boundaryGap" $ T.pointToForeign a

hoverAnimationEnabled ∷ ∀ i. Boolean → DSL (hoverAnimation ∷ I|i)
hoverAnimationEnabled a = set "hoverAnimation" $ toForeign a

showSymbol ∷ ∀ i. Boolean → DSL (showSymbol ∷ I|i)
showSymbol a = set "showSymbol" $ toForeign a

selectedMode ∷ ∀ i. T.SelectedMode → DSL (selectedMode ∷ I|i)
selectedMode a = set "selectedMode" $ T.selectedModeToForeign a

label ∷ ∀ i. DSL TP.LabelI → DSL (label ∷ I|i)
label = set "label" <<< buildObj

normalLabel ∷ ∀ i. DSL TP.LabelInnerI → DSL (normalLabel ∷ I|i)
normalLabel = set "normal" <<< buildObj

emphasisLabel ∷ ∀ i. DSL TP.LabelInnerI → DSL (emphasisLabel ∷ I|i)
emphasisLabel = set "emphasis" <<< buildObj

selected ∷ ∀ i. Boolean → DSL (selected ∷ I|i)
selected a = set "selected" $ toForeign a

leftPosition ∷ ∀ i. T.HorizontalPosition → DSL (left ∷ I|i)
leftPosition a = set "left" $ T.horizontalPositionToForeign a

alignLeft ∷ ∀ i. DSL (align ∷ I|i)
alignLeft = set "align" $ toForeign "left"

alignRight ∷ ∀ i. DSL (align ∷ I|i)
alignRight = set "align" $ toForeign "right"

alignAuto ∷ ∀ i. DSL (align ∷ I|i)
alignAuto = set "align" $ toForeign "auto"

brush ∷ ∀ i. DSL TP.BrushI → DSL (brush ∷ I|i)
brush = set "brush" <<< buildObj

brushToolbox ∷ ∀ i. DSL TP.BrushToolboxI → DSL (brushToolbox ∷ I|i)
brushToolbox a =  set "toolbox" $ buildArr a

rect ∷ ∀ i. DSL (tool ∷ I|i)
rect = set "" $ toForeign "rect"

polygon ∷ ∀ i. DSL (tool ∷ I|i)
polygon = set "" $ toForeign "polygon"

lineX ∷ ∀ i. DSL (tool ∷ I|i)
lineX = set "" $ toForeign "lineX"

lineY ∷ ∀ i. DSL (tool ∷ I|i)
lineY = set "" $ toForeign "lineY"

keep ∷ ∀ i. DSL (tool ∷ I|i)
keep = set "" $ toForeign "keep"

clear ∷ ∀ i. DSL (tool ∷ I|i)
clear = set "" $ toForeign "clear"

toolbox ∷ ∀ i. DSL TP.ToolboxI → DSL (toolbox ∷ I|i)
toolbox a = set "toolbox" $ buildObj a

feature ∷ ∀ i. DSL TP.FeatureI → DSL (feature ∷ I|i)
feature a = set "feature" $ buildObj a

magicType ∷ ∀ i. DSL TP.MagicTypeI → DSL (magicType ∷ I|i)
magicType a = set "magicType" $ buildObj a

magics ∷ ∀ i. DSL TP.MagicsI → DSL (magics ∷ I|i)
magics a = set "type" $ buildArr a

magicLine ∷ ∀ i. DSL (magic ∷ I|i)
magicLine = set "" $ toForeign "line"

magicBar ∷ ∀ i. DSL (magic ∷ I|i)
magicBar = set "" $ toForeign "bar"

magicStack ∷ ∀ i. DSL (magic ∷ I|i)
magicStack = set "" $ toForeign "stack"

magicTiled ∷ ∀ i. DSL (magic ∷ I|i)
magicTiled = set "" $ toForeign "tiled"

dataView ∷ ∀ i. DSL TP.DataViewI → DSL (dataView ∷ I|i)
dataView a = set "dataView" $ buildObj a

splitArea ∷ ∀ i. DSL TP.SplitAreaI → DSL (splitArea ∷ I|i)
splitArea a = set "splitArea" $ buildObj a

axisLine ∷ ∀ i. DSL TP.AxisLineI → DSL (axisLine ∷ I|i)
axisLine a = set "axisLine" $ buildObj a

silent ∷ ∀ i. Boolean → DSL (silent ∷ I|i)
silent a = set "silent" $ toForeign a

onZero ∷ ∀ i. Boolean → DSL (onZero ∷ I|i)
onZero a = set "onZero" $ toForeign a

inverse ∷ ∀ i. Boolean → DSL (inverse ∷ I|i)
inverse a = set "inverse" $ toForeign a

visualMap ∷ ∀ i. DSL TP.VisualMapI → DSL (visualMap ∷ I|i)
visualMap a = set "visualMap" $ buildSeries a

continuous ∷ ∀ i. DSL TP.ContinuousVisualMapI → DSL (continuousVisualMap ∷ I|i)
continuous a = set "continuous" $ buildObj a

dimension ∷ ∀ i. Int → DSL (dimension ∷ I|i)
dimension a = set "dimension" $ toForeign a

textPair ∷ ∀ i. String → String → DSL (textPair ∷ I|i)
textPair high low = set "text" $ toForeign [high, low]

itemHeight ∷ ∀ i. Number → DSL (itemHeight ∷ I|i)
itemHeight a = set "itemHeight" $ toForeign a

calculable ∷ ∀ i. Boolean → DSL (calculable ∷ I|i)
calculable a = set "calculable" $ toForeign a

min ∷ ∀ i. Number → DSL (min ∷ I|i)
min a = set "min" $ toForeign a

max ∷ ∀ i. Number → DSL (max ∷ I|i)
max a = set "max" $ toForeign a

inRange ∷ ∀ i. DSL TP.InOutRangeI → DSL (inRange ∷ I|i)
inRange a = set "inRagne" $ buildObj a

outOfRange ∷ ∀ i. DSL TP.InOutRangeI → DSL (outOfRange ∷ I|i)
outOfRange a = set "outOfRange" $ buildObj a

controller ∷ ∀ i. DSL TP.ControllerI → DSL (controller ∷ I|i)
controller a = set "controller" $ buildObj a

colorLightness ∷ ∀ i. Number → Number → DSL (colorLightness ∷ I|i)
colorLightness a b = set "colorLightness" $ toForeign [a, b]

itemStyle ∷ ∀ i. DSL TP.ItemStyleI → DSL (itemStyle ∷ I|i)
itemStyle a = set "itemStyle" $ buildObj a

normalItemStyle ∷ ∀ i. DSL TP.IStyleI → DSL (normalItemStyle ∷ I|i)
normalItemStyle a = set "normal" $ buildObj a

emphasisItemStyle ∷ ∀ i. DSL TP.IStyleI → DSL (emphasisItemStyle ∷ I|i)
emphasisItemStyle a = set "emphasis" $ buildObj a

barBorderWidth ∷ ∀ i. Number → DSL (barBorderWidth ∷ I|i)
barBorderWidth a = set "barBorderWidth" $ toForeign a

shadowBlur ∷ ∀ i. Number → DSL (shadowBlur ∷ I|i)
shadowBlur a = set "shadowBlur" $ toForeign a

shadowOffsetX ∷ ∀ i. Number → DSL (shadowOffsetX ∷ I|i)
shadowOffsetX a = set "shadowOffsetX" $ toForeign a

shadowOffsetY ∷ ∀ i. Number → DSL (shadowOffsetY ∷ I|i)
shadowOffsetY a = set "shadowOffsetY" $ toForeign a

shadowColor ∷ ∀ i. C.Color → DSL (shadowColor ∷ I|i)
shadowColor a = set "shadowColor" $ toForeign $ C.cssStringRGBA a

restore ∷ ∀ i. DSL TP.RestoreI → DSL (restore ∷ I|i)
restore = set "restore" <<< buildObj

saveAsImage ∷ ∀ i. DSL TP.SaveAsImageI → DSL (saveAsImage ∷ I|i)
saveAsImage = set "saveAsImage" <<< buildObj

z ∷ ∀ i. Int → DSL (z ∷ I|i)
z = set "z" <<< toForeign

splitNumber ∷ ∀ i. Int → DSL (splitNumber ∷ I|i)
splitNumber = set "splitNumber" <<< toForeign

gaugeRadius ∷ ∀ i. T.PixelOrPercent → DSL (gaugeRadius ∷ I|i)
gaugeRadius = set "radius" <<< T.pixelOrPercentToForeign

detail ∷ ∀ i. DSL TP.DetailI → DSL (detail ∷ I|i)
detail = set "detail" <<< buildObj

endAngle ∷ ∀ i. Number → DSL (endAngle ∷ I|i)
endAngle = set "endAngle" <<< toForeign

gaugePointer ∷ ∀ i. DSL TP.GaugePointerI → DSL (gaugePointer ∷ I|i)
gaugePointer = set "pointer" <<< buildObj

length ∷ ∀ i. Int → DSL (length ∷ I|i)
length = set "length" <<< toForeign

autoColor ∷ ∀ i. DSL (color ∷ I|i)
autoColor = set "color" $ toForeign "auto"

bolderFontWeight ∷ ∀ i. DSL (fontWeight ∷ I|i)
bolderFontWeight = set "fontWeight" $ toForeign "bolder"

fontSize ∷ ∀ i. Int → DSL (fontSize ∷ I|i)
fontSize = set "fontSize" <<< toForeign

italicFontStyle ∷ ∀ i. DSL (fontStyle ∷ I|i)
italicFontStyle = set "fontStyle" $ toForeign "italic"

offsetCenter ∷ ∀ i. T.Point → DSL (offsetCenter ∷ I|i)
offsetCenter = set "offsetCenter" <<< T.pointToForeign

subtext ∷ ∀ i. String → DSL (subtext ∷ I|i)
subtext = set "subtext" <<< toForeign

readOnly ∷ ∀ i. Boolean → DSL (readOnly ∷ I|i)
readOnly = set "readOnly" <<< toForeign

positionInside ∷ ∀ i. DSL (position ∷ I|i)
positionInside = set "position" $ toForeign "inside"

labelLine ∷ ∀ i. DSL TP.LabelLineI → DSL (labelLine ∷ I|i)
labelLine = set "labelLine" <<< buildObj

normalLabelLine ∷ ∀ i. DSL TP.LabelLineInnerI → DSL (normalLabelLine ∷ I|i)
normalLabelLine = set "normal" <<< buildObj

emphasisLabelLine ∷ ∀ i. DSL TP.LabelLineInnerI → DSL (emphasisLabelLine ∷ I|i)
emphasisLabelLine = set "emphasis" <<< buildObj

opacity ∷ ∀ i. Number → DSL (opacity ∷ I|i)
opacity = set "opacity" <<< toForeign

maxSize ∷ ∀ i. Int → DSL (maxSize ∷ I|i)
maxSize = set "maxSize" <<< toForeign

maxSizePct ∷ ∀ i. Number → DSL (maxSize ∷ I|i)
maxSizePct = set "maxSize" <<< toForeign <<< (_ <> "%") <<< show

borderColor ∷ ∀ i. C.Color → DSL (borderColor ∷ I|i)
borderColor = set "borderColor" <<< toForeign <<< C.toHexString

borderWidth ∷ ∀ i. Int → DSL (borderWidth ∷ I|i)
borderWidth = set "borderWidth" <<< toForeign
