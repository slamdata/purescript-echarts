module ECharts.Commands where

import Prelude

import Color as C
import Data.Array as Arr
import Data.Date (Date, year, month, day)
import Data.Enum (fromEnum)
import Data.Traversable as F
import Data.Foreign (toForeign, Foreign)
import ECharts.Monad (DSL, set, buildObj, buildSeries, buildArr, get, lastWithKeys)
import ECharts.Types as T
import ECharts.Types.Phantom (I, R)
import ECharts.Types.Phantom as TP
import ECharts.Internal (undefinedValue)

series ∷ ∀ i m. Monad m ⇒ DSL TP.SeriesI m → DSL (series ∷ I|i) m
series a = set "series" =<< buildSeries a

tooltip ∷ ∀ i m. Monad m ⇒ DSL TP.TooltipI m → DSL (tooltip ∷ I|i) m
tooltip a = set "tooltip" =<< buildObj a

grids ∷ ∀ i m. Monad m ⇒ DSL TP.GridsI m → DSL (grid ∷ I|i) m
grids = set "grid" <=< buildArr

grid ∷ ∀ i m. Monad m ⇒ DSL TP.GridI m → DSL (grid ∷ I|i) m
grid a = set "grid" =<< buildObj a

polar ∷ ∀ i m. Monad m ⇒ DSL TP.PolarI m → DSL (polar ∷ I|i) m
polar a = set "polar" =<< buildObj a

legend ∷ ∀ i m. Monad m ⇒ DSL TP.LegendI m → DSL (legend ∷ I|i) m
legend a = set "legend" =<< buildObj a

xAxis ∷ ∀ i m. Monad m ⇒ DSL TP.XAxisI m → DSL (xAxis ∷ I|i) m
xAxis a = set "xAxis" =<< buildObj a

yAxis ∷ ∀ i m. Monad m ⇒ DSL TP.YAxisI m → DSL (yAxis ∷ I|i) m
yAxis a = set "yAxis" =<< buildObj a

radiusAxis ∷ ∀ i m. Monad m ⇒ DSL TP.RadiusAxisI m → DSL (radiusAxis ∷ I|i) m
radiusAxis a = set "radiusAxis" =<< buildObj a

angleAxis ∷ ∀ i m. Monad m ⇒ DSL TP.AngleAxisI m → DSL (angleAxis ∷ I|i) m
angleAxis a = set "angleAxis" =<< buildObj a

color ∷ ∀ i m. Monad m ⇒ C.Color → DSL (color ∷ I|i) m
color a = set "color" $ toForeign $ C.toHexString a

colors ∷ ∀ i f m. Monad m ⇒ F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i) m
colors a = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.toHexString) a

rgbaColors ∷ ∀ i f m. Monad m ⇒ F.Foldable f ⇒ f C.Color → DSL (color ∷ I|i) m
rgbaColors a = set "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.cssStringRGBA) a

rgbaColor ∷ ∀ i m. Monad m ⇒ C.Color → DSL (color ∷ I|i) m
rgbaColor a = set "color" $ toForeign $ C.cssStringRGBA a

backgroundColor ∷ ∀ i m. Monad m ⇒ C.Color → DSL (backgroundColor ∷ I|i) m
backgroundColor a = set "backgroundColor" $ toForeign $ C.toHexString a

visible ∷ ∀ i m. Monad m ⇒ Boolean → DSL (show ∷ I|i) m
visible a = set "show" $ toForeign a

shown ∷ ∀ i m. Monad m ⇒ DSL (show ∷ I|i) m
shown = visible true

hidden ∷ ∀ i m. Monad m ⇒ DSL (show ∷ I|i) m
hidden = visible false

showTitle ∷ ∀ i m. Monad m ⇒ Boolean → DSL (showTitle ∷ I|i) m
showTitle a = set "showTitle" $ toForeign a

textStyle ∷ ∀ i m. Monad m ⇒ DSL TP.TextStyleI m → DSL (textStyle ∷ I|i) m
textStyle a = set "textStyle" =<< buildObj a

subtextStyle ∷ ∀ i m. Monad m ⇒ DSL TP.TextStyleI m → DSL (subtextStyle ∷ I|i) m
subtextStyle a = set "subtextStyle" =<< buildObj a

left ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (left ∷ I|i) m
left a = set "left" $ T.pixelOrPercentToForeign a

right ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (right ∷ I|i) m
right a = set "right" $ T.pixelOrPercentToForeign a

top ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (top ∷ I|i) m
top a = set "top" $ T.pixelOrPercentToForeign a

topTop ∷ ∀ i m. Monad m ⇒ DSL (top ∷ I|i) m
topTop = set "top" $ toForeign "top"

topMiddle ∷ ∀ i m. Monad m ⇒ DSL (top ∷ I|i) m
topMiddle = set "top" $ toForeign "middle"

topBottom ∷ ∀ i m. Monad m ⇒ DSL (top ∷ I|i) m
topBottom = set "top" $ toForeign "bottom"

bottom ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (bottom ∷ I|i) m
bottom a = set "bottom" $ T.pixelOrPercentToForeign a

bottomPx ∷ ∀ i m. Monad m ⇒ Int → DSL (bottom ∷ I|i) m
bottomPx = set "bottom" <<< toForeign

orient ∷ ∀ i m. Monad m ⇒ T.Orient → DSL (orient ∷ I|i) m
orient a = set "orient" $ T.orientToForeign a

items ∷ ∀ i f m. Monad m ⇒ F.Foldable f ⇒ f T.Item → DSL (items ∷ I|i) m
items a = set "data" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

itemsDSL ∷ ∀ i f m. Monad m ⇒ F.Traversable f ⇒ f (DSL TP.ItemI m) → DSL (items ∷ I|i) m
itemsDSL a = do
  is ← F.for a $ buildObj
  set "data" $ toForeign is

addItem ∷ ∀ i m. Monad m ⇒ DSL TP.ItemI m → DSL (item ∷ I|i) m
addItem = set "" <=< buildObj

buildItems ∷ ∀ i m. Monad m ⇒ DSL TP.ItemsI m → DSL (items ∷ I|i) m
buildItems = set "data" <=< buildArr

buildMarkItems ∷ ∀ i m. Monad m ⇒ DSL TP.ItemsI m → DSL (markItems ∷ I|i) m
buildMarkItems is = set "data" $ toForeign [ buildArr is ]

calendarIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (calendarIndex ∷ I|i) m
calendarIndex i = set "calendarIndex" $ toForeign i

visibleContent ∷ ∀ i m. Monad m ⇒ Boolean → DSL (showContent ∷ I|i) m
visibleContent a = set "showContent" $ toForeign a

showContent ∷ ∀ i m. Monad m ⇒ DSL (showContent ∷ I|i) m
showContent = visibleContent true

hideContent ∷ ∀ i m. Monad m ⇒ DSL (showContent ∷ I|i) m
hideContent = visibleContent false

alwaysShowContent ∷ ∀ i m. Monad m ⇒ Boolean → DSL (alwaysShowContent ∷ I|i) m
alwaysShowContent a = set "alwaysShowContent" $ toForeign a

trigger ∷ ∀ i m. Monad m ⇒ T.TooltipTrigger → DSL (trigger ∷ I|i) m
trigger a = set "trigger" $ T.tooltipTriggerToForeign a

triggerOnMouseMove ∷ ∀ i m. Monad m ⇒ DSL (triggerOn ∷ I|i) m
triggerOnMouseMove = set "triggerOn" $ toForeign "mousemove"

triggerOnClick ∷ ∀ i m. Monad m ⇒ DSL (triggerOn ∷ I|i) m
triggerOnClick = set "triggerOn" $ toForeign "click"

triggerAxis ∷ ∀ i m. Monad m ⇒ DSL (trigger ∷ I|i) m
triggerAxis = set "trigger" $ toForeign "axis"

triggerItem ∷ ∀ i m. Monad m ⇒ DSL (trigger ∷ I|i) m
triggerItem = set "trigger" $ toForeign "item"

triggerEvent ∷ ∀ i m. Monad m ⇒ Boolean → DSL (trigger ∷ I|i) m
triggerEvent a = set "triggerEvent" $ toForeign a

pie ∷ ∀ i m. Monad m ⇒ DSL TP.PieSeriesI m → DSL (pie ∷ I|i) m
pie = set "pie" <=< buildObj

line ∷ ∀ i m. Monad m ⇒ DSL TP.LineSeriesI m → DSL (line ∷ I|i) m
line = set "line" <=< buildObj

bar ∷ ∀ i m. Monad m ⇒ DSL TP.BarSeriesI m → DSL (bar ∷ I|i) m
bar = set "bar" <=< buildObj

scatter ∷ ∀ i m. Monad m ⇒ DSL TP.ScatterI m → DSL (scatter ∷ I|i) m
scatter = set "scatter" <=< buildObj

effectScatter ∷ ∀ i m. Monad m ⇒ DSL TP.EffectScatterI m → DSL (effectScatter ∷ I|i) m
effectScatter = set "effectScatter" <=< buildObj

treeMap ∷ ∀ i m. Monad m ⇒ DSL TP.TreeMapI m → DSL (treeMap ∷ I|i) m
treeMap = set "treemap" <=< buildObj

boxPlot ∷ ∀ i m. Monad m ⇒ DSL TP.BoxPlotI m → DSL (boxPlot ∷ I|i) m
boxPlot = set "boxplot" <=< buildObj

candlestick ∷ ∀ i m. Monad m ⇒ DSL TP.CandlestickI m → DSL (candlestick ∷ I|i) m
candlestick = set "candlestick" <=< buildObj

heatMap ∷ ∀ i m. Monad m ⇒ DSL TP.HeatMapI m → DSL (heatMap ∷ I|i) m
heatMap = set "heatmap" <=< buildObj

calendarSpec ∷ ∀ i m. Monad m ⇒ DSL TP.CalendarSpecI m → DSL (calendarSpec ∷ I|i) m
calendarSpec = set "calendar" <=< buildObj

map_ ∷ ∀ i m. Monad m ⇒ DSL TP.MapI m → DSL (map ∷ I|i) m
map_ = set "map" <=< buildObj

parallels ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelsI m → DSL (parallel ∷ I|i) m
parallels = set "parallel" <=< buildArr

parallel ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelI m → DSL (parallel ∷ I|i) m
parallel = set "parallel" <=< buildObj

lines ∷ ∀ i m. Monad m ⇒ DSL TP.LinesI m → DSL (lines ∷ I|i) m
lines = set "lines" <=< buildObj

graph ∷ ∀ i m. Monad m ⇒ DSL TP.GraphI m → DSL (graph ∷ I|i) m
graph = set "graph" <=< buildObj

sankey ∷ ∀ i m. Monad m ⇒ DSL TP.SankeyI m → DSL (sankey ∷ I|i) m
sankey = set "sankey" <=< buildObj

funnel ∷ ∀ i m. Monad m ⇒ DSL TP.FunnelI m → DSL (funnel ∷ I|i) m
funnel = set "funnel" <=< buildObj

parallelSeries ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelSeriesI m → DSL (parallelSeries ∷ I|i) m
parallelSeries = set "parallel" <=< buildObj

gauge ∷ ∀ i m. Monad m ⇒ DSL TP.GaugeI m → DSL (gauge ∷ I|i) m
gauge = set "gauge" <=< buildObj

radarSeries ∷ ∀ i m. Monad m ⇒ DSL TP.RadarSeriesI m → DSL (radarSeries ∷ I|i) m
radarSeries = set "radar" <=< buildObj

xAxisIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (xAxisIndex ∷ I|i) m
xAxisIndex a = set "xAxisIndex" $ toForeign a

yAxisIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (yAxisIndex ∷ I|i) m
yAxisIndex a = set "yAxisIndex" $ toForeign a

xAxisAllIndices ∷ ∀ i m. Monad m ⇒ DSL (xAxisIndex ∷ I|i) m
xAxisAllIndices = set "xAxisIndex" $ toForeign "all"

yAxisAllIndices ∷ ∀ i m. Monad m ⇒ DSL (yAxisIndex ∷ I|i) m
yAxisAllIndices = set "yAxisIndex" $ toForeign "all"

polarIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (polarIndex ∷ I|i) m
polarIndex a = set "polarIndex" $ toForeign a

symbol ∷ ∀ i m. Monad m ⇒ T.Symbol → DSL (symbol ∷ I|i) m
symbol a = set "symbol" $ T.symbolToForeign a

symbolSize ∷ ∀ i m. Monad m ⇒ Int → DSL (symbolSize ∷ I|i) m
symbolSize a = set "symbolSize" $ toForeign a

smooth ∷ ∀ i m. Monad m ⇒ Boolean → DSL (smooth ∷ I|i) m
smooth a = set "smooth" $ toForeign a

name ∷ ∀ i m. Monad m ⇒ String → DSL (name ∷ I|i) m
name a = set "name" $ toForeign a

stack ∷ ∀ i m. Monad m ⇒ String → DSL (stack ∷ I|i) m
stack a = set "stack" $ toForeign a

center ∷ ∀ i m. Monad m ⇒ T.Point → DSL (center ∷ I|i) m
center a = set "center" $ T.pointToForeign a

radius ∷ ∀ i m. Monad m ⇒ T.Radius → DSL (radius ∷ I|i) m
radius a = set "radius" $ T.radiusToForeign a

singleValueRadius ∷ ∀ i m. Monad m ⇒ T.SingleValueRadius → DSL (radius ∷ I|i) m
singleValueRadius a = set "radius" $ T.singleValueRadiusToForeign a

startAngle ∷ ∀ i m. Monad m ⇒ Number → DSL (startAngle ∷ I|i) m
startAngle a = set "startAngle" $ toForeign a

axisTick ∷ ∀ i m. Monad m ⇒ DSL TP.AxisTickI m → DSL (axisTick ∷ I|i) m
axisTick = set "axisTick" <=< buildObj

axisLabel ∷ ∀ i m. Monad m ⇒ DSL TP.AxisLabelI m → DSL (axisLabel ∷ I|i) m
axisLabel = set "axisLabel" <=< buildObj

axisType ∷ ∀ i m. Monad m ⇒ T.AxisType → DSL (axisType ∷ I|i) m
axisType a = set "type" $ T.axisTypeToForeign a

value ∷ ∀ i m. Monad m ⇒ Number → DSL (value ∷ I|i) m
value a = set "value" $ toForeign a

values ∷ ∀ i f m. Monad m ⇒ F.Foldable f ⇒ f Number → DSL (value ∷ I|i) m
values = set "value" <<< toForeign <<< F.foldMap Arr.singleton

buildValues ∷ ∀ i m. Monad m ⇒ DSL TP.ValuesI m → DSL (value ∷ I|i) m
buildValues = set "value" <=< buildArr

addValue ∷ ∀ i m. Monad m ⇒ Number → DSL (addValue ∷ I|i) m
addValue = set "" <<< toForeign

addStringValue ∷ ∀ i m. Monad m ⇒ String → DSL (addValue ∷ I|i) m
addStringValue = set "" <<< toForeign

autoValue ∷ ∀ i m. Monad m ⇒ DSL (addValue ∷ I|i) m
autoValue = set "" $ toForeign "auto"

buildNames ∷ ∀ i m. Monad m ⇒ DSL TP.NamesI m → DSL (name ∷ I|i) m
buildNames = set "name" <=< buildArr

addName ∷ ∀ i m. Monad m ⇒ String → DSL (addName ∷ I|i) m
addName = set "" <<< toForeign

missingValue ∷ ∀ i m. Monad m ⇒ DSL (addValue ∷ I|i) m
missingValue = set "" undefinedValue

missingName ∷ ∀ i m. Monad m ⇒ DSL (addName ∷ I|i) m
missingName = set "" undefinedValue

valuePair ∷ ∀ i m. Monad m ⇒ String → Number → DSL (value ∷ I|i) m
valuePair a b = set "value" $ toForeign [toForeign a, toForeign b]

titles ∷ ∀ i m. Monad m ⇒ DSL TP.TitlesI m → DSL (title ∷ I|i) m
titles = set "title" <=< buildArr

title ∷ ∀ i m. Monad m ⇒ DSL TP.TitleI m → DSL (title ∷ I|i) m
title = set "title" <=< buildObj

text ∷ ∀ i m. Monad m ⇒ String → DSL (text ∷ I|i) m
text a = set "text" $ toForeign a

showDelay ∷ ∀ i m. Monad m ⇒ Number → DSL (showDelay ∷ I|i) m
showDelay a = set "showDelay" $ toForeign a

hideDelay ∷ ∀ i m. Monad m ⇒ Number → DSL (hideDelay ∷ I|i) m
hideDelay a = set "hideDelay" $ toForeign a

pointerType ∷ ∀ i m. Monad m ⇒ T.PointerType → DSL (pointerType ∷ I|i) m
pointerType a = set "type" $ T.pointerTypeToForeign a

zlevel ∷ ∀ i m. Monad m ⇒ Int → DSL (zlevel ∷ I|i) m
zlevel a = set "zlevel" $ toForeign a

lineType ∷ ∀ i m. Monad m ⇒ T.LineType → DSL (lineType ∷ I|i) m
lineType a = set "type" $ toForeign a

width ∷ ∀ i m. Monad m ⇒ Int → DSL (width ∷ I|i) m
width a = set "width" $ toForeign a

widthPct ∷ ∀ i m. Monad m ⇒ Number → DSL (width ∷ I|i) m
widthPct = set "width" <<< toForeign <<< (_ <> "%") <<< show

axisPointer ∷ ∀ i m. Monad m ⇒ DSL TP.AxisPointerI m → DSL (axisPointer ∷ I|i) m
axisPointer = set "axisPointer" <=< buildObj

scale ∷ ∀ i m. Monad m ⇒ Boolean → DSL (scale ∷ I|i) m
scale a = set "scale" $ toForeign a

large ∷ ∀ i m. Monad m ⇒ Boolean → DSL (large ∷ I|i) m
large a = set "large" $ toForeign a

formatterAxis ∷ ∀ i m. Monad m ⇒ (Array T.FormatterInput → String) → DSL (formatter ∷ I|i) m
formatterAxis a = set "formatter" $ toForeign a

formatterAxisArrayValue
  ∷ ∀ i m. Monad m ⇒ (Array T.FormatterInputArrayValue → String) → DSL (formatter ∷ I|i) m
formatterAxisArrayValue a = set "formatter" $ toForeign a

formatterItem
  ∷ ∀ i m. Monad m ⇒ (T.FormatterInput → String) → DSL (formatter ∷ I|i) m
formatterItem a = set "formatter" $ toForeign a

formatterItemArrayValue
  ∷ ∀ i m. Monad m ⇒ (T.FormatterInputArrayValue → String) → DSL (formatter ∷ I|i) m
formatterItemArrayValue a = set "formatter" $ toForeign a

formatterString ∷ ∀ i m. Monad m ⇒ String → DSL (formatter ∷ I|i) m
formatterString a = set "formatter" $ toForeign a

formatterValue ∷ ∀ i m. Monad m ⇒ (Number → String) → DSL (formatter ∷ I|i) m
formatterValue = set "formatter" <<< toForeign

animationEnabled ∷ ∀ i m. Monad m ⇒ Boolean → DSL (animation ∷ I|i) m
animationEnabled a = set "animation" $ toForeign a

splitLine ∷ ∀ i m. Monad m ⇒ DSL TP.SplitLineI m → DSL (splitLine ∷ I|i) m
splitLine = set "splitLine" <=< buildObj

boundaryGap ∷ ∀ i m. Monad m ⇒ T.Point → DSL (boundaryGap ∷ I|i) m
boundaryGap a = set "boundaryGap" $ T.pointToForeign a

disabledBoundaryGap ∷ ∀ i m. Monad m ⇒ DSL (boundaryGap ∷ I|i) m
disabledBoundaryGap = set "boundaryGap" $ toForeign false

enabledBoundaryGap ∷ ∀ i m. Monad m ⇒ DSL (boundaryGap ∷ I|i) m
enabledBoundaryGap = set "boundaryGap" $ toForeign true

hoverAnimationEnabled ∷ ∀ i m. Monad m ⇒ Boolean → DSL (hoverAnimation ∷ I|i) m
hoverAnimationEnabled a = set "hoverAnimation" $ toForeign a

showSymbol ∷ ∀ i m. Monad m ⇒ Boolean → DSL (showSymbol ∷ I|i) m
showSymbol a = set "showSymbol" $ toForeign a

selectedMode ∷ ∀ i m. Monad m ⇒ T.SelectedMode → DSL (selectedMode ∷ I|i) m
selectedMode a = set "selectedMode" $ T.selectedModeToForeign a

label ∷ ∀ i m. Monad m ⇒ DSL TP.LabelI m → DSL (label ∷ I|i) m
label = set "label" <=< buildObj

normalLabel ∷ ∀ i m. Monad m ⇒ DSL TP.LabelInnerI m → DSL (normal ∷ R TP.LabelInnerI|i) m
normalLabel = normal

precision ∷ ∀ i m. Monad m ⇒ Number → DSL (precision ∷ I|i) m
precision = set "precision" <<< toForeign

emphasisLabel ∷ ∀ i m. Monad m ⇒ DSL TP.LabelInnerI m → DSL (emphasis ∷ R TP.LabelInnerI|i) m
emphasisLabel = emphasis

selected ∷ ∀ i m. Monad m ⇒ Boolean → DSL (selected ∷ I|i) m
selected a = set "selected" $ toForeign a

leftPosition ∷ ∀ i m. Monad m ⇒ T.HorizontalPosition → DSL (left ∷ I|i) m
leftPosition a = set "left" $ T.horizontalPositionToForeign a

alignLeft ∷ ∀ i m. Monad m ⇒ DSL (align ∷ I|i) m
alignLeft = set "align" $ toForeign "left"

alignRight ∷ ∀ i m. Monad m ⇒ DSL (align ∷ I|i) m
alignRight = set "align" $ toForeign "right"

alignAuto ∷ ∀ i m. Monad m ⇒ DSL (align ∷ I|i) m
alignAuto = set "align" $ toForeign "auto"

funnelLeft ∷ ∀ i m. Monad m ⇒ DSL (funnelAlign ∷ I|i) m
funnelLeft = set "funnelAlign" $ toForeign "left"

funnelRight ∷ ∀ i m. Monad m ⇒ DSL (funnelAlign ∷ I|i) m
funnelRight = set "funnelAlign" $ toForeign "right"

funnelCenter ∷ ∀ i m. Monad m ⇒ DSL (funnelAlign ∷ I|i) m
funnelCenter = set "funnelAlign" $ toForeign "center"

textLeft ∷ ∀ i m. Monad m ⇒ DSL (textAlign ∷ I|i) m
textLeft = set "textAlign" $ toForeign "left"

textRight ∷ ∀ i m. Monad m ⇒ DSL (textAlign ∷ I|i) m
textRight = set "textAlign" $ toForeign "right"

textCenter ∷ ∀ i m. Monad m ⇒ DSL (textAlign ∷ I|i) m
textCenter = set "textAlign" $ toForeign "center"

textTop ∷ ∀ i m. Monad m ⇒ DSL (textBaseline ∷ I|i) m
textTop = set "textBaseline" $ toForeign "top"

textBottom ∷ ∀ i m. Monad m ⇒ DSL (textBaseline ∷ I|i) m
textBottom = set "textBaseline" $ toForeign "bottom"

textMiddle ∷ ∀ i m. Monad m ⇒ DSL (textBaseline ∷ I|i) m
textMiddle = set "textBaseline" $ toForeign "middle"

brush ∷ ∀ i m. Monad m ⇒ DSL TP.BrushI m → DSL (brush ∷ I|i) m
brush = set "brush" <=< buildObj

brushType ∷ ∀ i m. Monad m ⇒ DSL TP.BrushToolboxI m → DSL (brushType ∷ I|i) m
brushType = set "type" <=< buildArr

brushToolbox ∷ ∀ i m. Monad m ⇒ DSL TP.BrushToolboxI m → DSL (brushToolbox ∷ I|i) m
brushToolbox = set "toolbox" <=< buildArr

brushModeSingle ∷ ∀ i m. Monad m ⇒ DSL (brushMode ∷ I|i) m
brushModeSingle = set "brushMode" $ toForeign "single"

brushIcons ∷ ∀ i m. Monad m ⇒ DSL TP.BFFieldI m → DSL (bfIcon ∷ I|i) m
brushIcons a = set "icon" =<< buildObj a

brushTitle ∷ ∀ i m. Monad m ⇒ DSL TP.BFFieldI m → DSL (bfTitle ∷ I|i) m
brushTitle a = set "title" =<< buildObj a

brushModeMultiple ∷ ∀ i m. Monad m ⇒ DSL (brushMode ∷ I|i) m
brushModeMultiple = set "brushMode" $ toForeign "multiple"

rect ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
rect = set "" $ toForeign "rect"

setRect ∷ ∀ i m. Monad m ⇒ String → DSL (rect ∷ I|i) m
setRect a = set "rect" $ toForeign a

polygon ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
polygon = set "" $ toForeign "polygon"

setPolygon ∷ ∀ i m. Monad m ⇒ String → DSL (polygon ∷ I|i) m
setPolygon a = set "polygon" $ toForeign a

lineX ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
lineX = set "" $ toForeign "lineX"

setLineX ∷ ∀ i m. Monad m ⇒ String → DSL (lineX ∷ I|i) m
setLineX a = set "lineX" $ toForeign a

lineY ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
lineY = set "" $ toForeign "lineY"

setLineY ∷ ∀ i m. Monad m ⇒ String → DSL (lineY ∷ I|i) m
setLineY a = set "lineY" $ toForeign a

keep ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
keep = set "" $ toForeign "keep"

setKeep ∷ ∀ i m. Monad m ⇒ String → DSL (keep ∷ I|i) m
setKeep a = set "keep" $ toForeign a

clear ∷ ∀ i m. Monad m ⇒ DSL (tool ∷ I|i) m
clear = set "" $ toForeign "clear"

setClear ∷ ∀ i m. Monad m ⇒ String → DSL (clear ∷ I|i) m
setClear a = set "clear" $ toForeign a

toolbox ∷ ∀ i m. Monad m ⇒ DSL TP.ToolboxI m → DSL (toolbox ∷ I|i) m
toolbox a = set "toolbox" =<< buildObj a

feature ∷ ∀ i m. Monad m ⇒ DSL TP.FeatureI m → DSL (feature ∷ I|i) m
feature a = set "feature" =<< buildObj a

brushFeature ∷ ∀ i m. Monad m ⇒ DSL TP.BrushFeatureI m → DSL (brush ∷ I|i) m
brushFeature a = set "brush" =<< buildObj a

magicType ∷ ∀ i m. Monad m ⇒ DSL TP.MagicTypeI m → DSL (magicType ∷ I|i) m
magicType a = set "magicType" =<< buildObj a

magics ∷ ∀ i m. Monad m ⇒ DSL TP.MagicsI m → DSL (magics ∷ I|i) m
magics a = set "type" =<< buildArr a

magicLine ∷ ∀ i m. Monad m ⇒ DSL (magic ∷ I|i) m
magicLine = set "" $ toForeign "line"

magicBar ∷ ∀ i m. Monad m ⇒ DSL (magic ∷ I|i) m
magicBar = set "" $ toForeign "bar"

magicStack ∷ ∀ i m. Monad m ⇒ DSL (magic ∷ I|i) m
magicStack = set "" $ toForeign "stack"

magicTiled ∷ ∀ i m. Monad m ⇒ DSL (magic ∷ I|i) m
magicTiled = set "" $ toForeign "tiled"

dataView ∷ ∀ i m. Monad m ⇒ DSL TP.DataViewI m → DSL (dataView ∷ I|i) m
dataView a = set "dataView" =<< buildObj a

splitArea ∷ ∀ i m. Monad m ⇒ DSL TP.SplitAreaI m → DSL (splitArea ∷ I|i) m
splitArea a = set "splitArea" =<< buildObj a

axisLine ∷ ∀ i m. Monad m ⇒ DSL TP.AxisLineI m → DSL (axisLine ∷ I|i) m
axisLine a = set "axisLine" =<< buildObj a

silent ∷ ∀ i m. Monad m ⇒ Boolean → DSL (silent ∷ I|i) m
silent a = set "silent" $ toForeign a

onZero ∷ ∀ i m. Monad m ⇒ Boolean → DSL (onZero ∷ I|i) m
onZero a = set "onZero" $ toForeign a

inverse ∷ ∀ i m. Monad m ⇒ Boolean → DSL (inverse ∷ I|i) m
inverse a = set "inverse" $ toForeign a

visualMap ∷ ∀ i m. Monad m ⇒ DSL TP.VisualMapI m → DSL (visualMap ∷ I|i) m
visualMap a = set "visualMap" =<< buildSeries a

calendar ∷ ∀ i m. Monad m ⇒ DSL TP.CalendarI m → DSL (calendar ∷ I|i) m
calendar a = set "calendar" =<< buildSeries a

continuous ∷ ∀ i m. Monad m ⇒ DSL TP.ContinuousVisualMapI m → DSL (continuousVisualMap ∷ I|i) m
continuous a = set "continuous" =<< buildObj a

dimension ∷ ∀ i m. Monad m ⇒ Int → DSL (dimension ∷ I|i) m
dimension a = set "dimension" $ toForeign a

textPair ∷ ∀ i m. Monad m ⇒ String → String → DSL (textPair ∷ I|i) m
textPair high low = set "text" $ toForeign [high, low]

itemHeight ∷ ∀ i m. Monad m ⇒ Number → DSL (itemHeight ∷ I|i) m
itemHeight a = set "itemHeight" $ toForeign a

itemWidth ∷ ∀ i m. Monad m ⇒ Number → DSL (itemWidth ∷ I|i) m
itemWidth a = set "itemWidth" $ toForeign a

calculable ∷ ∀ i m. Monad m ⇒ Boolean → DSL (calculable ∷ I|i) m
calculable a = set "calculable" $ toForeign a

min ∷ ∀ i m. Monad m ⇒ Number → DSL (min ∷ I|i) m
min a = set "min" $ toForeign a

max ∷ ∀ i m. Monad m ⇒ Number → DSL (max ∷ I|i) m
max a = set "max" $ toForeign a

inRange ∷ ∀ i m. Monad m ⇒ DSL TP.InOutRangeI m → DSL (inRange ∷ I|i) m
inRange a = set "inRange" =<< buildObj a

outOfRange ∷ ∀ i m. Monad m ⇒ DSL TP.InOutRangeI m → DSL (outOfRange ∷ I|i) m
outOfRange a = set "outOfRange" =<< buildObj a

controller ∷ ∀ i m. Monad m ⇒ DSL TP.ControllerI m → DSL (controller ∷ I|i) m
controller a = set "controller" =<< buildObj a

colorLightness ∷ ∀ i m. Monad m ⇒ Number → Number → DSL (colorLightness ∷ I|i) m
colorLightness a b = set "colorLightness" $ toForeign [a, b]

itemStyle ∷ ∀ i m. Monad m ⇒ DSL TP.ItemStyleI m → DSL (itemStyle ∷ I|i) m
itemStyle a = set "itemStyle" =<< buildObj a

normalItemStyle ∷ ∀ i m. Monad m ⇒ DSL TP.IStyleI m → DSL (normal ∷ R TP.IStyleI|i) m
normalItemStyle = normal

emphasisItemStyle ∷ ∀ i m. Monad m ⇒ DSL TP.IStyleI m → DSL (emphasis ∷ R TP.IStyleI|i) m
emphasisItemStyle = emphasis

barBorderWidth ∷ ∀ i m. Monad m ⇒ Number → DSL (barBorderWidth ∷ I|i) m
barBorderWidth a = set "barBorderWidth" $ toForeign a

shadowBlur ∷ ∀ i m. Monad m ⇒ Number → DSL (shadowBlur ∷ I|i) m
shadowBlur a = set "shadowBlur" $ toForeign a

shadowOffsetX ∷ ∀ i m. Monad m ⇒ Number → DSL (shadowOffsetX ∷ I|i) m
shadowOffsetX a = set "shadowOffsetX" $ toForeign a

shadowOffsetY ∷ ∀ i m. Monad m ⇒ Number → DSL (shadowOffsetY ∷ I|i) m
shadowOffsetY a = set "shadowOffsetY" $ toForeign a

shadowColor ∷ ∀ i m. Monad m ⇒ C.Color → DSL (shadowColor ∷ I|i) m
shadowColor a = set "shadowColor" $ toForeign $ C.cssStringRGBA a

restore ∷ ∀ i m. Monad m ⇒ DSL TP.RestoreI m → DSL (restore ∷ I|i) m
restore = set "restore" <=< buildObj

saveAsImage ∷ ∀ i m. Monad m ⇒ DSL TP.SaveAsImageI m → DSL (saveAsImage ∷ I|i) m
saveAsImage = set "saveAsImage" <=< buildObj

z ∷ ∀ i m. Monad m ⇒ Int → DSL (z ∷ I|i) m
z = set "z" <<< toForeign

splitNumber ∷ ∀ i m. Monad m ⇒ Int → DSL (splitNumber ∷ I|i) m
splitNumber = set "splitNumber" <<< toForeign

gaugeRadius ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (gaugeRadius ∷ I|i) m
gaugeRadius = set "radius" <<< T.pixelOrPercentToForeign

detail ∷ ∀ i m. Monad m ⇒ DSL TP.DetailI m → DSL (detail ∷ I|i) m
detail = set "detail" <=< buildObj

endAngle ∷ ∀ i m. Monad m ⇒ Number → DSL (endAngle ∷ I|i) m
endAngle = set "endAngle" <<< toForeign

gaugePointer ∷ ∀ i m. Monad m ⇒ DSL TP.GaugePointerI m → DSL (gaugePointer ∷ I|i) m
gaugePointer = set "pointer" <=< buildObj

length ∷ ∀ i m. Monad m ⇒ Int → DSL (length ∷ I|i) m
length = set "length" <<< toForeign

autoColor ∷ ∀ i m. Monad m ⇒ DSL (color ∷ I|i) m
autoColor = set "color" $ toForeign "auto"

bolderFontWeight ∷ ∀ i m. Monad m ⇒ DSL (fontWeight ∷ I|i) m
bolderFontWeight = set "fontWeight" $ toForeign "bolder"

fontSize ∷ ∀ i m. Monad m ⇒ Int → DSL (fontSize ∷ I|i) m
fontSize = set "fontSize" <<< toForeign

italicFontStyle ∷ ∀ i m. Monad m ⇒ DSL (fontStyle ∷ I|i) m
italicFontStyle = set "fontStyle" $ toForeign "italic"

offsetCenter ∷ ∀ i m. Monad m ⇒ T.Point → DSL (offsetCenter ∷ I|i) m
offsetCenter = set "offsetCenter" <<< T.pointToForeign

subtext ∷ ∀ i m. Monad m ⇒ String → DSL (subtext ∷ I|i) m
subtext = set "subtext" <<< toForeign

readOnly ∷ ∀ i m. Monad m ⇒ Boolean → DSL (readOnly ∷ I|i) m
readOnly = set "readOnly" <<< toForeign

positionInside ∷ ∀ i m. Monad m ⇒ DSL (position ∷ I|i) m
positionInside = set "position" $ toForeign "inside"

positionTop ∷ ∀ i m. Monad m ⇒ DSL (position ∷ I|i) m
positionTop = set "position" $ toForeign "top"

labelLine ∷ ∀ i m. Monad m ⇒ DSL TP.LabelLineI m → DSL (labelLine ∷ I|i) m
labelLine = set "labelLine" <=< buildObj

normalLabelLine ∷ ∀ i m. Monad m ⇒ DSL TP.LabelLineInnerI m → DSL (normal ∷ R TP.LabelLineInnerI|i) m
normalLabelLine = normal

emphasisLabelLine
  ∷ ∀ i m. Monad m ⇒ DSL TP.LabelLineInnerI m → DSL (emphasis ∷ R TP.LabelLineInnerI|i) m
emphasisLabelLine = emphasis

opacity ∷ ∀ i m. Monad m ⇒ Number → DSL (opacity ∷ I|i) m
opacity = set "opacity" <<< toForeign

maxSize ∷ ∀ i m. Monad m ⇒ Int → DSL (maxSize ∷ I|i) m
maxSize = set "maxSize" <<< toForeign

maxSizePct ∷ ∀ i m. Monad m ⇒ Number → DSL (maxSize ∷ I|i) m
maxSizePct = set "maxSize" <<< toForeign <<< (_ <> "%") <<< show

borderColor ∷ ∀ i m. Monad m ⇒ C.Color → DSL (borderColor ∷ I|i) m
borderColor = set "borderColor" <<< toForeign <<< C.toHexString

borderWidth ∷ ∀ i m. Monad m ⇒ Int → DSL (borderWidth ∷ I|i) m
borderWidth = set "borderWidth" <<< toForeign

normalLineStyle ∷ ∀ i m. Monad m ⇒ DSL TP.LineStyleI m → DSL (normal ∷ R TP.LineStyleI|i) m
normalLineStyle = normal

emphasisLineStyle ∷ ∀ i m. Monad m ⇒ DSL TP.LineStyleI m → DSL (emphasis ∷ R TP.LineStyleI|i) m
emphasisLineStyle = emphasis

leftCenter ∷ ∀ i m. Monad m ⇒ DSL (left ∷ I|i) m
leftCenter = set "left" $ toForeign "center"

leftLeft ∷ ∀ i m. Monad m ⇒ DSL (left ∷ I|i) m
leftLeft = set "left" $ toForeign "left"

leftRight ∷ ∀ i m. Monad m ⇒ DSL (left ∷ I|i) m
leftRight = set "left" $ toForeign "right"

itemGap ∷ ∀ i m. Monad m ⇒ Int → DSL (itemGap ∷ I|i) m
itemGap = set "itemGap" <<< toForeign

indicators ∷ ∀ i m. Monad m ⇒ DSL TP.IndicatorsI m → DSL (indicators ∷ I|i) m
indicators = set "indicator" <=< buildArr

indicator ∷ ∀ i m. Monad m ⇒ DSL TP.IndicatorI m → DSL (indicator ∷ I|i) m
indicator = set "" <=< buildObj

radarName ∷ ∀ i m. Monad m ⇒ DSL TP.RadarNameI m → DSL (radarName ∷ I|i) m
radarName = set "name" <=< buildObj

nameGap ∷ ∀ i m. Monad m ⇒ Number → DSL (nameGap ∷ I|i) m
nameGap = set "nameGap" <<< toForeign

polygonShape ∷ ∀ i m. Monad m ⇒ DSL (shape ∷ I|i) m
polygonShape = set "shape" $ toForeign "polygon"

circleShape ∷ ∀ i m. Monad m ⇒ DSL (shape ∷ I|i) m
circleShape = set "shape" $ toForeign "circle"

lineStylePair ∷ ∀ i m. Monad m ⇒ DSL TP.LineStylePairI m → DSL (lineStyle ∷ R TP.LineStylePairI|i) m
lineStylePair = lineStyle

areaStylePair ∷ ∀ i m. Monad m ⇒ DSL TP.AreaStylePairI m → DSL (areaStyle ∷ R TP.AreaStylePairI|i) m
areaStylePair = areaStyle

normalAreaStyle ∷ ∀ i m. Monad m ⇒ DSL TP.AreaStyleI m → DSL (normal ∷ R TP.AreaStyleI|i) m
normalAreaStyle = normal

emphasisAreaStyle ∷ ∀ i m. Monad m ⇒ DSL TP.AreaStyleI m → DSL (emphasis ∷ R TP.AreaStyleI|i) m
emphasisAreaStyle = emphasis

radars ∷ ∀ i m. Monad m ⇒ DSL TP.RadarsI m → DSL (radar ∷ I|i) m
radars = set "radar" <=< buildArr

radar ∷ ∀ i m. Monad m ⇒ DSL TP.RadarI m → DSL (radar ∷ I|i) m
radar = set "radar" <=< buildObj

ascending ∷ ∀ i m. Monad m ⇒ DSL (sort ∷ I|i) m
ascending = set "sort" $ toForeign "ascending"

descending ∷ ∀ i m. Monad m ⇒ DSL (sort ∷ I|i) m
descending = set "sort" $ toForeign "descending"

animationDurationUpdate ∷ ∀ i m. Monad m ⇒ Int → DSL (animationDurationUpdate ∷ I|i) m
animationDurationUpdate = set "animationDurationUpdate" <<< toForeign

animationEasingUpdateQuinticInOut ∷ ∀ i m. Monad m ⇒ DSL (animationEasingUpdate ∷ I|i) m
animationEasingUpdateQuinticInOut = set "animationEasingUpdate" $ toForeign "quinticInOut"

roam ∷ ∀ i m. Monad m ⇒ Boolean → DSL (roam ∷ I|i) m
roam = set "roam" <<< toForeign

edgeSymbols ∷ ∀ i m. Monad m ⇒ DSL TP.EdgeSymbolsI m → DSL (edgeSymbols ∷ I|i) m
edgeSymbols = set "edgeSymbol" <=< buildArr

circleEdgeSymbol ∷ ∀ i m. Monad m ⇒ DSL (edgeSymbol ∷ I|i) m
circleEdgeSymbol = set "" $ toForeign "circle"

arrowEdgeSymbol ∷ ∀ i m. Monad m ⇒ DSL (edgeSymbol ∷ I|i) m
arrowEdgeSymbol = set "" $ toForeign "arrow"

edgeSymbolSize ∷ ∀ i m. Monad m ⇒ Int → DSL (edgeSymbolSize ∷ I|i) m
edgeSymbolSize = set "edgeSymbolSize" <<< toForeign

edgeSymbolSizes ∷ ∀ i m. Monad m ⇒ Int → Int → DSL (edgeSymbolSize ∷ I|i) m
edgeSymbolSizes a b = set "edgeSymbolSize" $ toForeign [a, b]

buildLinks ∷ ∀ i m. Monad m ⇒ DSL TP.LinksI m → DSL (links ∷ I|i) m
buildLinks = set "links" <=< buildArr

addLink ∷ ∀ i m. Monad m ⇒ DSL TP.LinkI m → DSL (link ∷ I|i) m
addLink = set "" <=< buildObj

links ∷ ∀ i m. Monad m ⇒ Array { source ∷ String, target ∷ String } → DSL (links ∷ I|i) m
links = set "links" <<< toForeign

edgeLabel ∷ ∀ i m. Monad m ⇒ DSL TP.EdgeLabelI m → DSL (edgeLabel ∷ I|i) m
edgeLabel = set "edgeLabel" <=< buildObj

normalEdgeLabel ∷ ∀ i m. Monad m ⇒ DSL TP.EdgeLabelInnerI m → DSL (normal ∷ R TP.EdgeLabelInnerI|i) m
normalEdgeLabel = normal

emphasisEdgeLabel ∷ ∀ i m. Monad m ⇒ DSL TP.EdgeLabelInnerI m → DSL (emphasis ∷ R TP.EdgeLabelInnerI|i) m
emphasisEdgeLabel = emphasis

x ∷ ∀ i m. Monad m ⇒ Number → DSL (x ∷ I|i) m
x = set "x" <<< toForeign

y ∷ ∀ i m. Monad m ⇒ Number → DSL (y ∷ I|i) m
y = set "y" <<< toForeign

curveness ∷ ∀ i m. Monad m ⇒ Number → DSL (curveness ∷ I|i) m
curveness = set "curveness" <<< toForeign

symbolSizes ∷ ∀ i m. Monad m ⇒ Int → Int → DSL (symbolSize ∷ I|i) m
symbolSizes a b = set "symbolSize" $ toForeign [a, b]

symbolSizeArrFunc ∷ ∀ i m. Monad m ⇒ (Array Number → Number) → DSL (symbolSize ∷ I|i) m
symbolSizeArrFunc fn = set "symbolSize" $ toForeign fn

sourceIx ∷ ∀ i m. Monad m ⇒ Int → DSL (source ∷ I|i) m
sourceIx = set "source" <<< toForeign

targetIx ∷ ∀ i m. Monad m ⇒ Int → DSL (target ∷ I|i) m
targetIx = set "target" <<< toForeign

sourceName ∷ ∀ i m. Monad m ⇒ String → DSL (source ∷ I|i) m
sourceName = set "source" <<< toForeign

targetName ∷ ∀ i m. Monad m ⇒ String → DSL (target ∷ I|i) m
targetName = set "target" <<< toForeign

subtargetName ∷ ∀ i m. Monad m ⇒ String → DSL (subtarget ∷ I|i) m
subtargetName = set "subtarget" <<< toForeign

layoutNone ∷ ∀ i m. Monad m ⇒ DSL (layout ∷ I|i) m
layoutNone = set "layout" $ toForeign "none"

layoutCircular ∷ ∀ i m. Monad m ⇒ DSL (layout ∷ I|i) m
layoutCircular = set "layout" $ toForeign "circular"

layoutForce ∷ ∀ i m. Monad m ⇒ DSL (layout ∷ I|i) m
layoutForce = set "layout" $ toForeign "force"

missingSeries ∷ ∀ i m. Monad m ⇒ DSL (missing ∷ I|i) m
missingSeries = set "" undefinedValue

missingItem ∷ ∀ i m. Monad m ⇒ DSL (item ∷ I|i) m
missingItem = set "" undefinedValue

rotate ∷ ∀ i m. Monad m ⇒ Number → DSL (rotate ∷ I|i) m
rotate = set "rotate" <<< toForeign

fontFamily ∷ ∀ i m. Monad m ⇒ String → DSL (fontFamily ∷ I|i) m
fontFamily = set "fontFamily" <<< toForeign

addParallelAxis ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelAxisI m → DSL (addParallelAxis ∷ I|i) m
addParallelAxis = set "" <=< buildObj

parallelAxes ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelAxesI m → DSL (parallelAxis ∷ I|i) m
parallelAxes = set "parallelAxis" <=< buildArr

parallelAxisDefault ∷ ∀ i m. Monad m ⇒ DSL TP.ParallelAxisI m → DSL (parallelAxisDefault ∷ I|i) m
parallelAxisDefault = set "parallelAxisDefault" <=< buildObj

yAxes ∷ ∀ i m. Monad m ⇒ DSL TP.YAxesI m → DSL (yAxis ∷ I|i) m
yAxes = set "yAxis" <=< buildArr

xAxes ∷ ∀ i m. Monad m ⇒ DSL TP.XAxesI m → DSL (xAxis ∷ I|i) m
xAxes = set "xAxis" <=< buildArr

addYAxis ∷ ∀ i m. Monad m ⇒ DSL TP.YAxisI m → DSL (addYAxis ∷ I|i) m
addYAxis = set "" <=< buildObj

addXAxis ∷ ∀ i m. Monad m ⇒ DSL TP.XAxisI m → DSL (addXAxis ∷ I|i) m
addXAxis = set "" <=< buildObj

interval ∷ ∀ i m. Monad m ⇒ Int → DSL (interval ∷ I|i) m
interval = set "interval" <<< toForeign

lineAxisPointer ∷ ∀ i m. Monad m ⇒ DSL (axisPointerType ∷ I|i) m
lineAxisPointer = set "type" $ toForeign "line"

crossAxisPointer ∷ ∀ i m. Monad m ⇒ DSL (axisPointerType ∷ I|i) m
crossAxisPointer = set "type" $ toForeign "cross"

solidLine ∷ ∀ i m. Monad m ⇒ DSL (lineType ∷ I|i) m
solidLine = set "type" $ toForeign "solid"

dashedLine ∷ ∀ i m. Monad m ⇒ DSL (lineType ∷ I|i) m
dashedLine = set "type" $ toForeign "dashed"

dottedLine ∷ ∀ i m. Monad m ⇒ DSL (lineType ∷ I|i) m
dottedLine = set "type" $ toForeign "dotted"

widthNum ∷ ∀ i m. Monad m ⇒ Number → DSL (width ∷ I|i) m
widthNum = set "width" <<< toForeign

crossStyle ∷ ∀ i m. Monad m ⇒ DSL TP.CrossStyleI m → DSL (crossStyle ∷ I|i) m
crossStyle = set "crossStyle" <=< buildObj

normal ∷ ∀ p i m. Monad m ⇒ DSL p m → DSL (normal ∷ R p |i) m
normal = set "normal" <=< buildObj

lineStyle ∷ ∀ ρ i m. Monad m ⇒ DSL ρ m → DSL (lineStyle ∷ R ρ |i) m
lineStyle = set "lineStyle" <=< buildObj

areaStyle ∷ ∀ ρ i m. Monad m ⇒ DSL ρ m → DSL (areaStyle ∷ R ρ |i) m
areaStyle = set "areaStyle" <=< buildObj

emphasis ∷ ∀ ρ i m. Monad m ⇒ DSL ρ m → DSL (emphasis ∷ R ρ|i) m
emphasis = set "emphasis" <=< buildObj

heightPixelOrPercent ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (height ∷ I|i) m
heightPixelOrPercent = set "height" <<< T.pixelOrPercentToForeign

heightPct ∷ ∀ i m. Monad m ⇒ Number → DSL (width ∷ I|i) m
heightPct = set "height" <<< toForeign <<< (_ <> "%") <<< show

widthPixelOrPercent ∷ ∀ i m. Monad m ⇒ T.PixelOrPercent → DSL (width ∷ I|i) m
widthPixelOrPercent = set "width" <<< T.pixelOrPercentToForeign

padding ∷ ∀ i m. Monad m ⇒ Number → DSL (padding ∷ I|i) m
padding = set "padding" <<< toForeign

enterable ∷ ∀ i m. Monad m ⇒ Boolean → DSL (enterable ∷ I|i) m
enterable = set "enterable" <<< toForeign

transitionDuration ∷ ∀ i m. Monad m ⇒ Number → DSL (transitionDuration ∷ I|i) m
transitionDuration = set "transitionDuration" <<< toForeign

extraCssText ∷ ∀ i m. Monad m ⇒ String → DSL (extraCssText ∷ I|i) m
extraCssText = set "extraCssText" <<< toForeign

gridIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (gridIndex ∷ I|i) m
gridIndex a = set "gridIndex" $ toForeign a

radarIndex ∷ ∀ i m. Monad m ⇒ Number → DSL (radarIndex ∷ I|i) m
radarIndex = set "radarIndex" <<< toForeign

parallelIndex ∷ ∀ i m. Monad m ⇒ Int → DSL (parallelIndex ∷ I|i) m
parallelIndex = set "parallelIndex" <<< toForeign

treeMapNodeId ∷ ∀ i m. Monad m ⇒ String → DSL (treeMapNodeId ∷ I|i) m
treeMapNodeId = set "id" <<< toForeign

visualDimension ∷ ∀ i m. Monad m ⇒ Int → DSL (visualDimension ∷ I|i) m
visualDimension = set "visualDimension" <<< toForeign

visibleMin ∷ ∀ i m. Monad m ⇒ Number → DSL (visibleMin ∷ I|i) m
visibleMin = set "visibleMin" <<< toForeign

childVisibleMin ∷ ∀ i m. Monad m ⇒ Number → DSL (childVisibleMin ∷ I|i) m
childVisibleMin = set "childVisibleMin" <<< toForeign

category ∷ ∀ i m. Monad m ⇒ Int → DSL (category ∷ I|i) m
category = set "category" <<< toForeign

coords ∷ ∀ i f m. Monad m ⇒ F.Foldable f ⇒ f T.Coord → DSL (coords ∷ I|i) m
coords a = set "coords" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

valueIndex ∷ ∀ i m. Monad m ⇒ Number → DSL (valueIndex ∷ I|i) m
valueIndex = set "valueIndex" <<< toForeign

valueDim ∷ ∀ i m. Monad m ⇒ String → DSL (valueDim ∷ I|i) m
valueDim = set "valueDim" <<< toForeign

markType ∷ ∀ i m. Monad m ⇒ String → DSL (markType ∷ I|i) m
markType = set "type" <<< toForeign

margin ∷ ∀ i m. Monad m ⇒ Int → DSL (margin ∷ I|i) m
margin = set "margin" <<< toForeign

markLine ∷ ∀ i m. Monad m ⇒ DSL TP.MarkLineI m → DSL (markLine ∷ I|i) m
markLine = set "markLine" <=< buildObj

markPoint ∷ ∀ i m. Monad m ⇒ DSL TP.MarkPointI m → DSL (markPoint ∷ I|i) m
markPoint = set "markPoint" <=< buildObj

markArea ∷ ∀ i m. Monad m ⇒ DSL TP.MarkAreaI m → DSL (markArea ∷ I|i) m
markArea = set "markArea" <=< buildObj

repulsion ∷ ∀ i m. Monad m ⇒ Number → DSL (repulsion ∷ I|i) m
repulsion = set "repulsion" <<< toForeign

gravity ∷ ∀ i m. Monad m ⇒ Number → DSL (gravity ∷ I|i) m
gravity = set "gravity" <<< toForeign

edgeLength ∷ ∀ i m. Monad m ⇒ Number → DSL (edgeLength ∷ I|i) m
edgeLength = set "edgeLength" <<< toForeign

edgeLengths ∷ ∀ i m. Monad m ⇒ Number → Number → DSL (edgeLength ∷ I|i) m
edgeLengths a b = set "edgeLength" $ toForeign [ a, b ]

layoutAnimation ∷ ∀ i m. Monad m ⇒ Boolean → DSL (layoutAnimation ∷ I|i) m
layoutAnimation = set "layoutAnimation" <<< toForeign

circular ∷ ∀ i m. Monad m ⇒ DSL TP.CircularI m → DSL (circular ∷ I|i) m
circular = set "circular" <=< buildObj

rotateLabel ∷ ∀ i m. Monad m ⇒ Boolean → DSL (rotateLabel ∷ I|i) m
rotateLabel = set "rotateLabel" <<< toForeign

force ∷ ∀ i m. Monad m ⇒ DSL TP.ForceI m → DSL (force ∷ I|i) m
force = set "force" <=< buildObj

buildCategories ∷ ∀ i m. Monad m ⇒ DSL TP.CategoriesI m → DSL (categories ∷ I|i) m
buildCategories = set "categories" <=< buildArr

addCategory ∷ ∀ i m. Monad m ⇒ DSL TP.CategoryI m → DSL (category ∷ I|i) m
addCategory = set "" <=< buildObj

colorSource ∷ ∀ i m. Monad m ⇒ DSL (color ∷ I|i) m
colorSource = set "color" $ toForeign "source"

colorTarget ∷ ∀ i m. Monad m ⇒ DSL (color ∷ I|i) m
colorTarget = set "target" $ toForeign "target"

buildCoord ∷ ∀ i m. Monad m ⇒ DSL TP.PointI m → DSL (coord ∷ I|i) m
buildCoord dsl = do
  xx ← get "x" dsl
  yy ← get "y" dsl
  set "coord" $ toForeign $ Arr.catMaybes [ xx, yy ]

buildCenter ∷ ∀ i m. Monad m ⇒ DSL TP.PointI m → DSL (center ∷ I|i) m
buildCenter dsl = do
  xx ← get "x" dsl
  yy ← get "y" dsl
  set "center" $ toForeign $ Arr.catMaybes [ xx, yy ]

buildRadius ∷ ∀ i m. Monad m ⇒ DSL TP.RadiusI m → DSL (radius ∷ I|i) m
buildRadius dsl = do
 s ← get "start" dsl
 e ← get "end" dsl
 set "radius" $ toForeign $ Arr.concat [s, e]

setStart ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (start ∷ I|i) m
setStart dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "start"

setEnd ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (end ∷ I|i) m
setEnd dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "end"

setBarWidth ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (barWidth ∷ I|i) m
setBarWidth dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "barWidth"

setX ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (x ∷ I|i) m
setX dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "x"

setY ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (y ∷ I|i) m
setY dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "y"

setZ ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (z ∷ I|i) m
setZ dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "z"

coordXIx ∷ ∀ i m. Monad m ⇒ Int → DSL (x ∷ I|i) m
coordXIx = set "x" <<< toForeign

coordXValue ∷ ∀ i m. Monad m ⇒ String → DSL (x ∷ I|i) m
coordXValue = set "x" <<< toForeign

coordY ∷ ∀ i m. Monad m ⇒ String → DSL (y ∷ I|i) m
coordY = set "y" <<< toForeign

pixels ∷ ∀ i m. Monad m ⇒ Int → DSL (pixels ∷ I|i) m
pixels = set "pixels" <<< toForeign

percents ∷ ∀ i m. Monad m ⇒ Number → DSL (percents ∷ I|i) m
percents = set "percents" <<< toForeign <<< (_ <> "%") <<< show

setWidth ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (width ∷ I|i) m
setWidth dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "width"

buildGaugeRadius ∷ ∀ i m. Monad m ⇒ DSL TP.DimensionI m → DSL (gaugeRadius ∷ I|i) m
buildGaugeRadius dsl = do
  keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set "radius"

buildOffsetCenter ∷ ∀ i m. Monad m ⇒ DSL TP.PointI m → DSL (offsetCenter ∷ I|i) m
buildOffsetCenter dsl = do
  xx ← get "x" dsl
  yy ← get "y" dsl
  set "offsetCenter" $ toForeign $ Arr.catMaybes [ xx, yy ]

containLabel ∷ ∀ i m. Monad m ⇒ Boolean → DSL (containLabel ∷ I|i) m
containLabel = set "containLabel" <<< toForeign

polarCoordinateSystem ∷ ∀ i m. Monad m ⇒ DSL (coordinateSystem ∷ I|i) m
polarCoordinateSystem = set "coordinateSystem" $ toForeign "polar"

cartesianCoordinateSystem ∷ ∀ i m. Monad m ⇒ DSL (coordinateSystem ∷ I|i) m
cartesianCoordinateSystem = set "coordinateSystem" $ toForeign "cartesian2d"

geoCoordinateSystem ∷ ∀ i m. Monad m ⇒ DSL (coordinateSystem ∷ I|i) m
geoCoordinateSystem = set "coordinateSystem" $ toForeign "geo"

calendarCoordinateSystem ∷ ∀ i m. Monad m ⇒ DSL (coordinateSystem ∷ I|i) m
calendarCoordinateSystem = set "coordinateSystem" $ toForeign "calendar"

dim ∷ ∀ i m. Monad m ⇒ Int → DSL (dim ∷ I|i) m
dim = set "dim" <<< toForeign

nameLocationStart ∷ ∀ i m. Monad m ⇒ DSL (nameLocation ∷ I|i) m
nameLocationStart = set "nameLocation" $ toForeign "start"

nameLocationEnd ∷ ∀ i m. Monad m ⇒ DSL (nameLocation ∷ I|i) m
nameLocationEnd = set "nameLocation" $ toForeign "end"

nameLocationMiddle ∷ ∀ i m. Monad m ⇒ DSL (nameLocation ∷ I|i) m
nameLocationMiddle = set "nameLocation" $ toForeign "middle"

nameTextStyle ∷ ∀ i m. Monad m ⇒ DSL TP.TextStyleI m → DSL (nameTextStyle ∷ I|i) m
nameTextStyle o = set "nameTextStyle" =<< buildObj o

nameRotate ∷ ∀ i m. Monad m ⇒ Number → DSL (nameRotate ∷ I|i) m
nameRotate o = set "nameRotate" $ toForeign o

buildCellSize ∷ ∀ i m. Monad m ⇒ DSL TP.ValuesI m → DSL (cellSize ∷ I|i) m
buildCellSize = set "cellSize" <=< buildArr

buildRange ∷ ∀ i m. Monad m ⇒ DSL TP.ValuesI m → DSL (range ∷ I|i) m
buildRange = set "range" <=< buildArr

addDateValue ∷ ∀ i m. Monad m ⇒ Date → DSL (addValue ∷ I|i) m
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
