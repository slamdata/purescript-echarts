module ECharts.Commands where

import Prelude

import Color as C

import Data.Array as Arr
import Data.Date (Date, year, month, day)
import Data.Enum (fromEnum)
import Data.Foldable as F
import Data.Foreign (toForeign)

import ECharts.Monad (DSL, set, buildObj, buildSeries, buildArr, get, lastWithKeys)
import ECharts.Types as T
import ECharts.Types.Phantom (I, R)
import ECharts.Types.Phantom as TP
import ECharts.Internal (undefinedValue)

series ∷ ∀ i. DSL TP.SeriesI → DSL (series ∷ I|i)
series a = set "series" $ buildSeries a

tooltip ∷ ∀ i. DSL TP.TooltipI → DSL (tooltip ∷ I|i)
tooltip a = set "tooltip" $ buildObj a

grids ∷ ∀ i. DSL TP.GridsI → DSL (grid ∷ I|i)
grids = set "grid" <<< buildArr

grid ∷ ∀ i. DSL TP.GridI → DSL (grid ∷ I|i)
grid a = set "grid" $ buildObj a

polar ∷ ∀ i. DSL TP.PolarI → DSL (polar ∷ I|i)
polar a = set "polar" $ buildObj a

legend ∷ ∀ i. DSL TP.LegendI → DSL (legend ∷ I|i)
legend a = set "legend" $ buildObj a

xAxis ∷ ∀ i. DSL TP.XAxisI → DSL (xAxis ∷ I|i)
xAxis a = set "xAxis" $ buildObj a

yAxis ∷ ∀ i. DSL TP.YAxisI → DSL (yAxis ∷ I|i)
yAxis a = set "yAxis" $ buildObj a

radiusAxis ∷ ∀ i. DSL TP.RadiusAxisI → DSL (radiusAxis ∷ I|i)
radiusAxis a = set "radiusAxis" $ buildObj a

angleAxis ∷ ∀ i. DSL TP.AngleAxisI → DSL (angleAxis ∷ I|i)
angleAxis a = set "angleAxis" $ buildObj a

color ∷ ∀ i. C.Color → DSL (color ∷ I|i)
color a = set "color" $ toForeign $ C.toHexString a

colors ∷ ∀ i f. F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i)
colors a = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.toHexString) a

rgbaColors ∷ ∀ i f. F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i)
rgbaColors a = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.cssStringRGBA) a

rgbaColor ∷ ∀ i. C.Color → DSL (color ∷ I|i)
rgbaColor a = set "color" $ toForeign $ C.cssStringRGBA a

backgroundColor ∷ ∀ i. C.Color → DSL (backgroundColor ∷ I|i)
backgroundColor a = set "backgroundColor" $ toForeign $ C.toHexString a

visible ∷ ∀ i. Boolean → DSL (show ∷ I|i)
visible a = set "show" $ toForeign a

shown ∷ ∀ i. DSL (show ∷ I|i)
shown = visible true

hidden ∷ ∀ i. DSL (show ∷ I|i)
hidden = visible false

showTitle ∷ ∀ i. Boolean → DSL (showTitle ∷ I|i)
showTitle a = set "showTitle" $ toForeign a

textStyle ∷ ∀ i. DSL TP.TextStyleI → DSL (textStyle ∷ I|i)
textStyle a = set "textStyle" $ buildObj a

subtextStyle ∷ ∀ i. DSL TP.TextStyleI → DSL (subtextStyle ∷ I|i)
subtextStyle a = set "subtextStyle" $ buildObj a

left ∷ ∀ i. T.PixelOrPercent → DSL (left ∷ I|i)
left a = set "left" $ T.pixelOrPercentToForeign a

right ∷ ∀ i. T.PixelOrPercent → DSL (right ∷ I|i)
right a = set "right" $ T.pixelOrPercentToForeign a

top ∷ ∀ i. T.PixelOrPercent → DSL (top ∷ I|i)
top a = set "top" $ T.pixelOrPercentToForeign a

topTop ∷ ∀ i. DSL (top ∷ I|i)
topTop = set "top" $ toForeign "top"

topMiddle ∷ ∀ i. DSL (top ∷ I|i)
topMiddle = set "top" $ toForeign "middle"

topBottom ∷ ∀ i. DSL (top ∷ I|i)
topBottom = set "top" $ toForeign "bottom"

bottom ∷ ∀ i. T.PixelOrPercent → DSL (bottom ∷ I|i)
bottom a = set "bottom" $ T.pixelOrPercentToForeign a

bottomPx ∷ ∀ i. Int → DSL (bottom ∷ I|i)
bottomPx = set "bottom" <<< toForeign

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

buildMarkItems ∷ ∀ i. DSL TP.ItemsI → DSL (markItems ∷ I|i)
buildMarkItems is = set "data" $ toForeign [ buildArr is ]

calendarIndex ∷ ∀ i. Int → DSL (calendarIndex ∷ I|i)
calendarIndex i = set "calendarIndex" $ toForeign i

visibleContent ∷ ∀ i. Boolean → DSL (showContent ∷ I|i)
visibleContent a = set "showContent" $ toForeign a

showContent ∷ ∀ i. DSL (showContent ∷ I|i)
showContent = visibleContent true

hideContent ∷ ∀ i. DSL (showContent ∷ I|i)
hideContent = visibleContent false

alwaysShowContent ∷ ∀ i. Boolean → DSL (alwaysShowContent ∷ I|i)
alwaysShowContent a = set "alwaysShowContent" $ toForeign a

trigger ∷ ∀ i. T.TooltipTrigger → DSL (trigger ∷ I|i)
trigger a = set "trigger" $ T.tooltipTriggerToForeign a

triggerOnMouseMove ∷ ∀ i. DSL (triggerOn ∷ I|i)
triggerOnMouseMove = set "triggerOn" $ toForeign "mousemove"

triggerOnClick ∷ ∀ i. DSL (triggerOn ∷ I|i)
triggerOnClick = set "triggerOn" $ toForeign "click"

triggerAxis ∷ ∀ i. DSL (trigger ∷ I|i)
triggerAxis = set "trigger" $ toForeign "axis"

triggerItem ∷ ∀ i. DSL (trigger ∷ I|i)
triggerItem = set "trigger" $ toForeign "item"

triggerEvent ∷ ∀ i. Boolean → DSL (trigger ∷ I|i)
triggerEvent a = set "triggerEvent" $ toForeign a

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

calendarSpec ∷ ∀ i. DSL TP.CalendarSpecI → DSL (calendarSpec ∷ I|i)
calendarSpec = set "calendar" <<< buildObj

map_ ∷ ∀ i. DSL TP.MapI → DSL (map ∷ I|i)
map_ = set "map" <<< buildObj

parallels ∷ ∀ i. DSL TP.ParallelsI → DSL (parallel ∷ I|i)
parallels = set "parallel" <<< buildArr

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

parallelSeries ∷ ∀ i. DSL TP.ParallelSeriesI → DSL (parallelSeries ∷ I|i)
parallelSeries = set "parallel" <<< buildObj

gauge ∷ ∀ i. DSL TP.GaugeI → DSL (gauge ∷ I|i)
gauge = set "gauge" <<< buildObj

radarSeries ∷ ∀ i. DSL TP.RadarSeriesI → DSL (radarSeries ∷ I|i)
radarSeries = set "radar" <<< buildObj

xAxisIndex ∷ ∀ i. Int → DSL (xAxisIndex ∷ I|i)
xAxisIndex a = set "xAxisIndex" $ toForeign a

yAxisIndex ∷ ∀ i. Int → DSL (yAxisIndex ∷ I|i)
yAxisIndex a = set "yAxisIndex" $ toForeign a

polarIndex ∷ ∀ i. Int → DSL (polarIndex ∷ I|i)
polarIndex a = set "polarIndex" $ toForeign a

symbol ∷ ∀ i. T.Symbol → DSL (symbol ∷ I|i)
symbol a = set "symbol" $ T.symbolToForeign a

symbolSize ∷ ∀ i. Int → DSL (symbolSize ∷ I|i)
symbolSize a = set "symbolSize" $ toForeign a

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

singleValueRadius ∷ ∀ i. T.SingleValueRadius → DSL (radius ∷ I|i)
singleValueRadius a = set "radius" $ T.singleValueRadiusToForeign a

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

values ∷ ∀ i f. F.Foldable f ⇒ f Number → DSL (value ∷ I|i)
values = set "value" <<< toForeign <<< F.foldMap Arr.singleton

buildValues ∷ ∀ i. DSL TP.ValuesI → DSL (value ∷ I|i)
buildValues = set "value" <<< buildArr

addValue ∷ ∀ i. Number → DSL (addValue ∷ I|i)
addValue = set "" <<< toForeign

addStringValue ∷ ∀ i. String → DSL (addValue ∷ I|i)
addStringValue = set "" <<< toForeign

autoValue ∷ ∀ i. DSL (addValue ∷ I|i)
autoValue = set "" $ toForeign "auto"

buildNames ∷ ∀ i. DSL TP.NamesI → DSL (name ∷ I|i)
buildNames = set "name" <<< buildArr

addName ∷ ∀ i. String → DSL (addName ∷ I|i)
addName = set "" <<< toForeign

missingValue ∷ ∀ i. DSL (addValue ∷ I|i)
missingValue = set "" undefinedValue

missingName ∷ ∀ i. DSL (addName ∷ I|i)
missingName = set "" undefinedValue

valuePair ∷ ∀ i. String → Number → DSL (value ∷ I|i)
valuePair a b = set "value" $ toForeign [toForeign a, toForeign b]

titles ∷ ∀ i. DSL TP.TitlesI → DSL (title ∷ I|i)
titles = set "title" <<< buildArr

title ∷ ∀ i. DSL TP.TitleI → DSL (title ∷ I|i)
title = set "title" <<< buildObj

text ∷ ∀ i. String → DSL (text ∷ I|i)
text a = set "text" $ toForeign a

showDelay ∷ ∀ i. Number → DSL (showDelay ∷ I|i)
showDelay a = set "showDelay" $ toForeign a

hideDelay ∷ ∀ i. Number → DSL (hideDelay ∷ I|i)
hideDelay a = set "hideDelay" $ toForeign a

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

formatterAxisArrayValue ∷ ∀ i. (Array T.FormatterInputArrayValue → String) → DSL (formatter ∷ I|i)
formatterAxisArrayValue a = set "formatter" $ toForeign a

formatterItem ∷ ∀ i. (T.FormatterInput → String) → DSL (formatter ∷ I|i)
formatterItem a = set "formatter" $ toForeign a

formatterItemArrayValue ∷ ∀ i. (T.FormatterInputArrayValue → String) → DSL (formatter ∷ I|i)
formatterItemArrayValue a = set "formatter" $ toForeign a

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

disabledBoundaryGap ∷ ∀ i. DSL (boundaryGap ∷ I|i)
disabledBoundaryGap = set "boundaryGap" $ toForeign false

enabledBoundaryGap ∷ ∀ i. DSL (boundaryGap ∷ I|i)
enabledBoundaryGap = set "boundaryGap" $ toForeign true

hoverAnimationEnabled ∷ ∀ i. Boolean → DSL (hoverAnimation ∷ I|i)
hoverAnimationEnabled a = set "hoverAnimation" $ toForeign a

showSymbol ∷ ∀ i. Boolean → DSL (showSymbol ∷ I|i)
showSymbol a = set "showSymbol" $ toForeign a

selectedMode ∷ ∀ i. T.SelectedMode → DSL (selectedMode ∷ I|i)
selectedMode a = set "selectedMode" $ T.selectedModeToForeign a

label ∷ ∀ i. DSL TP.LabelI → DSL (label ∷ I|i)
label = set "label" <<< buildObj

normalLabel ∷ ∀ i. DSL TP.LabelInnerI → DSL (normal ∷ R TP.LabelInnerI|i)
normalLabel = normal

precision ∷ ∀ i. Number → DSL (precision ∷ I|i)
precision = set "precision" <<< toForeign

emphasisLabel ∷ ∀ i. DSL TP.LabelInnerI → DSL (emphasis ∷ R TP.LabelInnerI|i)
emphasisLabel = emphasis

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

funnelLeft ∷ ∀ i. DSL (funnelAlign ∷ I|i)
funnelLeft = set "funnelAlign" $ toForeign "left"

funnelRight ∷ ∀ i. DSL (funnelAlign ∷ I|i)
funnelRight = set "funnelAlign" $ toForeign "right"

funnelCenter ∷ ∀ i. DSL (funnelAlign ∷ I|i)
funnelCenter = set "funnelAlign" $ toForeign "center"

textLeft ∷ ∀ i. DSL (textAlign ∷ I|i)
textLeft = set "textAlign" $ toForeign "left"

textRight ∷ ∀ i. DSL (textAlign ∷ I|i)
textRight = set "textAlign" $ toForeign "right"

textCenter ∷ ∀ i. DSL (textAlign ∷ I|i)
textCenter = set "textAlign" $ toForeign "center"

textTop ∷ ∀ i. DSL (textBaseline ∷ I|i)
textTop = set "textBaseline" $ toForeign "top"

textBottom ∷ ∀ i. DSL (textBaseline ∷ I|i)
textBottom = set "textBaseline" $ toForeign "bottom"

textMiddle ∷ ∀ i. DSL (textBaseline ∷ I|i)
textMiddle = set "textBaseline" $ toForeign "middle"

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

calendar ∷ ∀ i. DSL TP.CalendarI → DSL (calendar ∷ I|i)
calendar a = set "calendar" $ buildSeries a

continuous ∷ ∀ i. DSL TP.ContinuousVisualMapI → DSL (continuousVisualMap ∷ I|i)
continuous a = set "continuous" $ buildObj a

dimension ∷ ∀ i. Int → DSL (dimension ∷ I|i)
dimension a = set "dimension" $ toForeign a

textPair ∷ ∀ i. String → String → DSL (textPair ∷ I|i)
textPair high low = set "text" $ toForeign [high, low]

itemHeight ∷ ∀ i. Number → DSL (itemHeight ∷ I|i)
itemHeight a = set "itemHeight" $ toForeign a

itemWidth ∷ ∀ i. Number → DSL (itemWidth ∷ I|i)
itemWidth a = set "itemWidth" $ toForeign a

calculable ∷ ∀ i. Boolean → DSL (calculable ∷ I|i)
calculable a = set "calculable" $ toForeign a

min ∷ ∀ i. Number → DSL (min ∷ I|i)
min a = set "min" $ toForeign a

max ∷ ∀ i. Number → DSL (max ∷ I|i)
max a = set "max" $ toForeign a

inRange ∷ ∀ i. DSL TP.InOutRangeI → DSL (inRange ∷ I|i)
inRange a = set "inRange" $ buildObj a

outOfRange ∷ ∀ i. DSL TP.InOutRangeI → DSL (outOfRange ∷ I|i)
outOfRange a = set "outOfRange" $ buildObj a

controller ∷ ∀ i. DSL TP.ControllerI → DSL (controller ∷ I|i)
controller a = set "controller" $ buildObj a

colorLightness ∷ ∀ i. Number → Number → DSL (colorLightness ∷ I|i)
colorLightness a b = set "colorLightness" $ toForeign [a, b]

itemStyle ∷ ∀ i. DSL TP.ItemStyleI → DSL (itemStyle ∷ I|i)
itemStyle a = set "itemStyle" $ buildObj a

normalItemStyle ∷ ∀ i. DSL TP.IStyleI → DSL (normal ∷ R TP.IStyleI|i)
normalItemStyle = normal

emphasisItemStyle ∷ ∀ i. DSL TP.IStyleI → DSL (emphasis ∷ R TP.IStyleI|i)
emphasisItemStyle = emphasis

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

positionTop ∷ ∀ i. DSL (position ∷ I|i)
positionTop = set "position" $ toForeign "top"

labelLine ∷ ∀ i. DSL TP.LabelLineI → DSL (labelLine ∷ I|i)
labelLine = set "labelLine" <<< buildObj

normalLabelLine ∷ ∀ i. DSL TP.LabelLineInnerI → DSL (normal ∷ R TP.LabelLineInnerI|i)
normalLabelLine = normal

emphasisLabelLine ∷ ∀ i. DSL TP.LabelLineInnerI → DSL (emphasis ∷ R TP.LabelLineInnerI|i)
emphasisLabelLine = emphasis

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

normalLineStyle ∷ ∀ i. DSL TP.LineStyleI → DSL (normal ∷ R TP.LineStyleI|i)
normalLineStyle = normal

emphasisLineStyle ∷ ∀ i. DSL TP.LineStyleI → DSL (emphasis ∷ R TP.LineStyleI|i)
emphasisLineStyle = emphasis

leftCenter ∷ ∀ i. DSL (left ∷ I|i)
leftCenter = set "left" $ toForeign "center"

leftLeft ∷ ∀ i. DSL (left ∷ I|i)
leftLeft = set "left" $ toForeign "left"

leftRight ∷ ∀ i. DSL (left ∷ I|i)
leftRight = set "left" $ toForeign "right"

itemGap ∷ ∀ i. Int → DSL (itemGap ∷ I|i)
itemGap = set "itemGap" <<< toForeign

indicators ∷ ∀ i. DSL TP.IndicatorsI → DSL (indicators ∷ I|i)
indicators = set "indicator" <<< buildArr

indicator ∷ ∀ i. DSL TP.IndicatorI → DSL (indicator ∷ I|i)
indicator = set "" <<< buildObj

radarName ∷ ∀ i. DSL TP.RadarNameI → DSL (radarName ∷ I|i)
radarName = set "name" <<< buildObj

nameGap ∷ ∀ i. Number → DSL (nameGap ∷ I|i)
nameGap = set "nameGap" <<< toForeign

polygonShape ∷ ∀ i. DSL (shape ∷ I|i)
polygonShape = set "shape" $ toForeign "polygon"

circleShape ∷ ∀ i. DSL (shape ∷ I|i)
circleShape = set "shape" $ toForeign "circle"

lineStylePair ∷ ∀ i. DSL TP.LineStylePairI → DSL (lineStyle ∷ R TP.LineStylePairI|i)
lineStylePair = lineStyle

areaStylePair ∷ ∀ i. DSL TP.AreaStylePairI → DSL (areaStyle ∷ R TP.AreaStylePairI|i)
areaStylePair = areaStyle

normalAreaStyle ∷ ∀ i. DSL TP.AreaStyleI → DSL (normal ∷ R TP.AreaStyleI|i)
normalAreaStyle = normal

emphasisAreaStyle ∷ ∀ i.DSL TP.AreaStyleI → DSL (emphasis ∷ R TP.AreaStyleI|i)
emphasisAreaStyle = emphasis

radars ∷ ∀ i. DSL TP.RadarsI → DSL (radar ∷ I|i)
radars = set "radar" <<< buildArr

radar ∷ ∀ i. DSL TP.RadarI → DSL (radar ∷ I|i)
radar = set "radar" <<< buildObj

ascending ∷ ∀ i. DSL (sort ∷ I|i)
ascending = set "sort" $ toForeign "ascending"

descending ∷ ∀ i. DSL (sort ∷ I|i)
descending = set "sort" $ toForeign "descending"

animationDurationUpdate ∷ ∀ i. Int → DSL (animationDurationUpdate ∷ I|i)
animationDurationUpdate = set "animationDurationUpdate" <<< toForeign

animationEasingUpdateQuinticInOut ∷ ∀ i. DSL (animationEasingUpdate ∷ I|i)
animationEasingUpdateQuinticInOut = set "animationEasingUpdate" $ toForeign "quinticInOut"

roam ∷ ∀ i. Boolean → DSL (roam ∷ I|i)
roam = set "roam" <<< toForeign

edgeSymbols ∷ ∀ i. DSL TP.EdgeSymbolsI → DSL (edgeSymbols ∷ I|i)
edgeSymbols = set "edgeSymbol" <<< buildArr

circleEdgeSymbol ∷ ∀ i. DSL (edgeSymbol ∷ I|i)
circleEdgeSymbol = set "" $ toForeign "circle"

arrowEdgeSymbol ∷ ∀ i. DSL (edgeSymbol ∷ I|i)
arrowEdgeSymbol = set "" $ toForeign "arrow"

edgeSymbolSize ∷ ∀ i. Int → DSL (edgeSymbolSize ∷ I|i)
edgeSymbolSize = set "edgeSymbolSize" <<< toForeign

edgeSymbolSizes ∷ ∀ i. Int → Int → DSL (edgeSymbolSize ∷ I|i)
edgeSymbolSizes a b = set "edgeSymbolSize" $ toForeign [a, b]

buildLinks ∷ ∀ i. DSL TP.LinksI → DSL (links ∷ I|i)
buildLinks = set "links" <<< buildArr

addLink ∷ ∀ i. DSL TP.LinkI → DSL (link ∷ I|i)
addLink = set "" <<< buildObj

links ∷ ∀ i. Array { source ∷ String, target ∷ String } → DSL (links ∷ I|i)
links = set "links" <<< toForeign

edgeLabel ∷ ∀ i. DSL TP.EdgeLabelI → DSL (edgeLabel ∷ I|i)
edgeLabel = set "edgeLabel" <<< buildObj

normalEdgeLabel ∷ ∀ i. DSL TP.EdgeLabelInnerI → DSL (normal ∷ R TP.EdgeLabelInnerI|i)
normalEdgeLabel = normal

emphasisEdgeLabel ∷ ∀ i. DSL TP.EdgeLabelInnerI → DSL (emphasis ∷ R TP.EdgeLabelInnerI|i)
emphasisEdgeLabel = emphasis

x ∷ ∀ i. Number → DSL (x ∷ I|i)
x = set "x" <<< toForeign

y ∷ ∀ i. Number → DSL (y ∷ I|i)
y = set "y" <<< toForeign

curveness ∷ ∀ i. Number → DSL (curveness ∷ I|i)
curveness = set "curveness" <<< toForeign

symbolSizes ∷ ∀ i. Int → Int → DSL (symbolSize ∷ I|i)
symbolSizes a b = set "symbolSize" $ toForeign [a, b]

symbolSizeArrFunc ∷ ∀ i. (Array Number → Number) → DSL (symbolSize ∷ I|i)
symbolSizeArrFunc fn = set "symbolSize" $ toForeign fn

sourceIx ∷ ∀ i. Int → DSL (source ∷ I|i)
sourceIx = set "source" <<< toForeign

targetIx ∷ ∀ i. Int → DSL (target ∷ I|i)
targetIx = set "target" <<< toForeign

sourceName ∷ ∀ i. String → DSL (source ∷ I|i)
sourceName = set "source" <<< toForeign

targetName ∷ ∀ i. String → DSL (target ∷ I|i)
targetName = set "target" <<< toForeign

subtargetName ∷ ∀ i. String → DSL (subtarget ∷ I|i)
subtargetName = set "subtarget" <<< toForeign

layoutNone ∷ ∀ i. DSL (layout ∷ I|i)
layoutNone = set "layout" $ toForeign "none"

layoutCircular ∷ ∀ i. DSL (layout ∷ I|i)
layoutCircular = set "layout" $ toForeign "circular"

layoutForce ∷ ∀ i. DSL (layout ∷ I|i)
layoutForce = set "layout" $ toForeign "force"

missingSeries ∷ ∀ i. DSL (missing ∷ I|i)
missingSeries = set "" undefinedValue

missingItem ∷ ∀ i. DSL (item ∷ I|i)
missingItem = set "" undefinedValue

rotate ∷ ∀ i. Number → DSL (rotate ∷ I|i)
rotate = set "rotate" <<< toForeign

fontFamily ∷ ∀ i. String → DSL (fontFamily ∷ I|i)
fontFamily = set "fontFamily" <<< toForeign

addParallelAxis ∷ ∀ i. DSL TP.ParallelAxisI → DSL (addParallelAxis ∷ I|i)
addParallelAxis = set "" <<< buildObj

parallelAxes ∷ ∀ i. DSL TP.ParallelAxesI → DSL (parallelAxis ∷ I|i)
parallelAxes = set "parallelAxis" <<< buildArr

parallelAxisDefault ∷ ∀ i. DSL TP.ParallelAxisI → DSL (parallelAxisDefault ∷ I|i)
parallelAxisDefault = set "parallelAxisDefault" <<< buildObj

yAxes ∷ ∀ i. DSL TP.YAxesI → DSL (yAxis ∷ I|i)
yAxes = set "yAxis" <<< buildArr

xAxes ∷ ∀ i. DSL TP.XAxesI → DSL (xAxis ∷ I|i)
xAxes = set "xAxis" <<< buildArr

addYAxis ∷ ∀ i. DSL TP.YAxisI → DSL (addYAxis ∷ I|i)
addYAxis = set "" <<< buildObj

addXAxis ∷ ∀ i. DSL TP.XAxisI → DSL (addXAxis ∷ I|i)
addXAxis = set "" <<< buildObj

interval ∷ ∀ i. Int → DSL (interval ∷ I|i)
interval = set "interval" <<< toForeign

lineAxisPointer ∷ ∀ i. DSL (axisPointerType ∷ I|i)
lineAxisPointer = set "type" $ toForeign "line"

crossAxisPointer ∷ ∀ i. DSL (axisPointerType ∷ I|i)
crossAxisPointer = set "type" $ toForeign "cross"

solidLine ∷ ∀ i. DSL (lineType ∷ I|i)
solidLine = set "type" $ toForeign "solid"

dashedLine ∷ ∀ i. DSL (lineType ∷ I|i)
dashedLine = set "type" $ toForeign "dashed"

dottedLine ∷ ∀ i. DSL (lineType ∷ I|i)
dottedLine = set "type" $ toForeign "dotted"

widthNum ∷ ∀ i. Number → DSL (width ∷ I|i)
widthNum = set "width" <<< toForeign

crossStyle ∷ ∀ i. DSL TP.CrossStyleI → DSL (crossStyle ∷ I|i)
crossStyle = set "crossStyle" <<< buildObj

normal ∷ ∀ p i. DSL p → DSL (normal ∷ R p |i)
normal = set "normal" <<< buildObj

lineStyle ∷ ∀ ρ i. DSL ρ → DSL (lineStyle ∷ R ρ |i)
lineStyle = set "lineStyle" <<< buildObj

areaStyle ∷ ∀ ρ i. DSL ρ → DSL (areaStyle ∷ R ρ |i)
areaStyle = set "areaStyle" <<< buildObj

emphasis ∷ ∀ p i. DSL p → DSL (emphasis ∷ R p|i)
emphasis = set "emphasis" <<< buildObj

heightPixelOrPercent ∷ ∀ i. T.PixelOrPercent → DSL (height ∷ I|i)
heightPixelOrPercent = set "height" <<< T.pixelOrPercentToForeign

heightPct ∷ ∀ i. Number → DSL (width ∷ I|i)
heightPct = set "height" <<< toForeign <<< (_ <> "%") <<< show

widthPixelOrPercent ∷ ∀ i. T.PixelOrPercent → DSL (width ∷ I|i)
widthPixelOrPercent = set "width" <<< T.pixelOrPercentToForeign

padding ∷ ∀ i. Number → DSL (padding ∷ I|i)
padding = set "padding" <<< toForeign

enterable ∷ ∀ i. Boolean → DSL (enterable ∷ I|i)
enterable = set "enterable" <<< toForeign

transitionDuration ∷ ∀ i. Number → DSL (transitionDuration ∷ I|i)
transitionDuration = set "transitionDuration" <<< toForeign

extraCssText ∷ ∀ i. String → DSL (extraCssText ∷ I|i)
extraCssText = set "extraCssText" <<< toForeign

gridIndex ∷ ∀ i. Int → DSL (gridIndex ∷ I|i)
gridIndex a = set "gridIndex" $ toForeign a

radarIndex ∷ ∀ i. Number → DSL (radarIndex ∷ I|i)
radarIndex = set "radarIndex" <<< toForeign

parallelIndex ∷ ∀ i. Int → DSL (parallelIndex ∷ I|i)
parallelIndex = set "parallelIndex" <<< toForeign

treeMapNodeId ∷ ∀ i. String → DSL (treeMapNodeId ∷ I|i)
treeMapNodeId = set "id" <<< toForeign

visualDimension ∷ ∀ i. Int → DSL (visualDimension ∷ I|i)
visualDimension = set "visualDimension" <<< toForeign

visibleMin ∷ ∀ i. Number → DSL (visibleMin ∷ I|i)
visibleMin = set "visibleMin" <<< toForeign

childVisibleMin ∷ ∀ i. Number → DSL (childVisibleMin ∷ I|i)
childVisibleMin = set "childVisibleMin" <<< toForeign

category ∷ ∀ i. Int → DSL (category ∷ I|i)
category = set "category" <<< toForeign

coords ∷ ∀ i f. F.Foldable f ⇒ f T.Coord → DSL (coords ∷ I|i)
coords a = set "coords" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

valueIndex ∷ ∀ i. Number → DSL (valueIndex ∷ I|i)
valueIndex = set "valueIndex" <<< toForeign

valueDim ∷ ∀ i. String → DSL (valueDim ∷ I|i)
valueDim = set "valueDim" <<< toForeign

markType ∷ ∀ i. String → DSL (markType ∷ I|i)
markType = set "type" <<< toForeign

margin ∷ ∀ i. Int → DSL (margin ∷ I|i)
margin = set "margin" <<< toForeign

markLine ∷ ∀ i. DSL TP.MarkLineI → DSL (markLine ∷ I|i)
markLine = set "markLine" <<< buildObj

markPoint ∷ ∀ i. DSL TP.MarkPointI → DSL (markPoint ∷ I|i)
markPoint = set "markPoint" <<< buildObj

markArea ∷ ∀ i. DSL TP.MarkAreaI → DSL (markArea ∷ I|i)
markArea = set "markArea" <<< buildObj

repulsion ∷ ∀ i. Number → DSL (repulsion ∷ I|i)
repulsion = set "repulsion" <<< toForeign

gravity ∷ ∀ i. Number → DSL (gravity ∷ I|i)
gravity = set "gravity" <<< toForeign

edgeLength ∷ ∀ i. Number → DSL (edgeLength ∷ I|i)
edgeLength = set "edgeLength" <<< toForeign

edgeLengths ∷ ∀ i. Number → Number → DSL (edgeLength ∷ I|i)
edgeLengths a b = set "edgeLength" $ toForeign [ a, b ]

layoutAnimation ∷ ∀ i. Boolean → DSL (layoutAnimation ∷ I|i)
layoutAnimation = set "layoutAnimation" <<< toForeign

circular ∷ ∀ i. DSL TP.CircularI → DSL (circular ∷ I|i)
circular = set "circular" <<< buildObj

rotateLabel ∷ ∀ i. Boolean → DSL (rotateLabel ∷ I|i)
rotateLabel = set "rotateLabel" <<< toForeign

force ∷ ∀ i. DSL TP.ForceI → DSL (force ∷ I|i)
force = set "force" <<< buildObj

buildCategories ∷ ∀ i. DSL TP.CategoriesI → DSL (categories ∷ I|i)
buildCategories is = set "categories" $ buildArr is

addCategory ∷ ∀ i. DSL TP.CategoryI → DSL (category ∷ I|i)
addCategory = set "" <<< buildObj

colorSource ∷ ∀ i. DSL (color ∷ I|i)
colorSource = set "color" $ toForeign "source"

colorTarget ∷ ∀ i. DSL (color ∷ I|i)
colorTarget = set "target" $ toForeign "target"

buildCoord ∷ ∀ i. DSL TP.PointI → DSL (coord ∷ I|i)
buildCoord dsl =
  let
    xx = get "x" dsl
    yy = get "y" dsl
  in
    set "coord" $ toForeign $ Arr.catMaybes [ xx, yy ]

buildCenter ∷ ∀ i. DSL TP.PointI → DSL (center ∷ I|i)
buildCenter dsl =
  let
    xx = get "x" dsl
    yy = get "y" dsl
  in
    set "center" $ toForeign $ Arr.catMaybes [ xx, yy ]

buildRadius ∷ ∀ i. DSL TP.RadiusI → DSL (radius ∷ I|i)
buildRadius dsl =
  let
    s = get "start" dsl
    e = get "end" dsl
  in
    set "radius" $ toForeign $ Arr.catMaybes [ s, e ]

setStart ∷ ∀ i. DSL TP.DimensionI → DSL (start ∷ I|i)
setStart dsl =
  F.traverse_ (set "start") $ lastWithKeys ["pixels", "percents"] dsl

setEnd ∷ ∀ i. DSL TP.DimensionI → DSL (end ∷ I|i)
setEnd dsl =
  F.traverse_ (set "end") $ lastWithKeys ["pixels", "percents"] dsl

setBarWidth ∷ ∀ i. DSL TP.DimensionI → DSL (barWidth ∷ I|i)
setBarWidth dsl =
  F.traverse_ (set "barWidth") $ lastWithKeys ["pixels", "percents"] dsl

setX ∷ ∀ i. DSL TP.DimensionI → DSL (x ∷ I|i)
setX dsl =
  F.traverse_ (set "x") $ lastWithKeys ["pixels", "percents"] dsl

setY ∷ ∀ i. DSL TP.DimensionI → DSL (y ∷ I|i)
setY dsl =
  F.traverse_ (set "y") $ lastWithKeys ["pixels", "percents"] dsl

setZ ∷ ∀ i. DSL TP.DimensionI → DSL (z ∷ I|i)
setZ dsl =
  F.traverse_ (set "z") $ lastWithKeys ["pixels", "percents"] dsl

coordXIx ∷ ∀ i. Int → DSL (x ∷ I|i)
coordXIx = set "x" <<< toForeign

coordXValue ∷ ∀ i. String → DSL (x ∷ I|i)
coordXValue = set "x" <<< toForeign

coordY ∷ ∀ i. String → DSL (y ∷ I|i)
coordY = set "y" <<< toForeign

pixels ∷ ∀ i. Int → DSL (pixels ∷ I|i)
pixels = set "pixels" <<< toForeign

percents ∷ ∀ i. Number → DSL (percents ∷ I|i)
percents = set "percents" <<< toForeign <<< (_ <> "%") <<< show

setWidth ∷ ∀ i. DSL TP.DimensionI → DSL (width ∷ I|i)
setWidth dsl =
  F.traverse_ (set "width") $ lastWithKeys ["pixels", "percents"] dsl

buildGaugeRadius ∷ ∀ i. DSL TP.DimensionI → DSL (gaugeRadius ∷ I|i)
buildGaugeRadius dsl =
  F.traverse_ (set "radius") $ lastWithKeys ["pixels", "percents"] dsl

buildOffsetCenter ∷ ∀ i. DSL TP.PointI → DSL (offsetCenter ∷ I|i)
buildOffsetCenter dsl =
  let
    xx = get "x" dsl
    yy = get "y" dsl
  in
    set "offsetCenter" $ toForeign $ Arr.catMaybes [ xx, yy ]

containLabel ∷ ∀ i. Boolean → DSL (containLabel ∷ I|i)
containLabel = set "containLabel" <<< toForeign

polarCoordinateSystem ∷ ∀ i. DSL (coordinateSystem ∷ I|i)
polarCoordinateSystem = set "coordinateSystem" $ toForeign "polar"

cartesianCoordinateSystem ∷ ∀ i. DSL (coordinateSystem ∷ I|i)
cartesianCoordinateSystem = set "coordinateSystem" $ toForeign "cartesian2d"

geoCoordinateSystem ∷ ∀ i. DSL (coordinateSystem ∷ I|i)
geoCoordinateSystem = set "coordinateSystem" $ toForeign "geo"

calendarCoordinateSystem ∷ ∀ i. DSL (coordinateSystem ∷ I|i)
calendarCoordinateSystem = set "coordinateSystem" $ toForeign "calendar"

dim ∷ ∀ i. Int → DSL (dim ∷ I|i)
dim = set "dim" <<< toForeign

nameLocationStart ∷ ∀ i. DSL (nameLocation ∷ I|i)
nameLocationStart = set "nameLocation" $ toForeign "start"

nameLocationEnd ∷ ∀ i. DSL (nameLocation ∷ I|i)
nameLocationEnd = set "nameLocation" $ toForeign "end"

buildCellSize ∷ ∀ i. DSL TP.ValuesI → DSL (cellSize ∷ I|i)
buildCellSize = set "cellSize" <<< buildArr

buildRange ∷ ∀ i. DSL TP.ValuesI → DSL (range ∷ I|i)
buildRange = set "range" <<< buildArr

addDateValue ∷ ∀ i. Date → DSL (addValue ∷ I|i)
addDateValue dt =
  set "" <<< toForeign
    $ year' dt
    <> "-"
    <> month' dt
    <> "-"
    <> day' dt
  where
    year' = show <<< fromEnum <<< year
    month' = show <<< fromEnum <<< month
    day' = show <<< fromEnum <<< day
