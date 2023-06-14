module ECharts.Commands where

import Prelude

import Color as C
import Data.Array as Arr
import Data.Date (Date, year, month, day)
import Data.Enum (fromEnum)
import Data.Traversable as F
import Data.Tuple (Tuple(..), snd, fst)
import Data.Foreign (toForeign, Foreign)
import ECharts.Monad (CommandsT, DSL, set, buildObj, buildSeries, buildArr, get, lastWithKeys, set')
import ECharts.Types as T
-- import ECharts.Types.Phantom (I, R)
-- import ECharts.Types.Phantom as TP
import ECharts.Internal (undefinedValue)

series ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
series a = set "series" =<< buildSeries a

tooltip ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
tooltip a = set "tooltip" =<< buildObj a

grids ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
grids = set "grid" <=< buildArr

grid ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
grid a = set "grid" =<< buildObj a

polar ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
polar a = set "polar" =<< buildObj a

legend ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
legend a = set "legend" =<< buildObj a

xAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
xAxis a = set "xAxis" =<< buildObj a

yAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
yAxis a = set "yAxis" =<< buildObj a

radiusAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
radiusAxis a = set "radiusAxis" =<< buildObj a

angleAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
angleAxis a = set "angleAxis" =<< buildObj a

color ∷ ∀ m. Monad m ⇒ C.Color → DSL m
color a = set' "color" $ toForeign $ C.toHexString a

colors ∷ ∀ f m. Monad m ⇒ F.Foldable f ⇒ f C.Color → DSL m
colors a = set' "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.toHexString) a

rgbaColors ∷ ∀ f m. Monad m ⇒ F.Foldable f ⇒ f C.Color → DSL m
rgbaColors a = set' "color" $ toForeign $ F.foldMap (Arr.singleton <<< C.cssStringRGBA) a

rgbaColor ∷ ∀ m. Monad m ⇒ C.Color → DSL m
rgbaColor a = set' "color" $ toForeign $ C.cssStringRGBA a

backgroundColor ∷ ∀ m. Monad m ⇒ C.Color → DSL m
backgroundColor a = set' "backgroundColor" $ toForeign $ C.toHexString a

visible ∷ ∀ m. Monad m ⇒ Boolean → DSL m
visible a = set' "show" $ toForeign a

shown ∷ ∀ m. Monad m ⇒ DSL m
shown = visible true

hidden ∷ ∀ m. Monad m ⇒ DSL m
hidden = visible false

showTitle ∷ ∀ m. Monad m ⇒ Boolean → DSL m
showTitle a = set' "showTitle" $ toForeign a

textStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
textStyle a = set "textStyle" =<< buildObj a

subtextStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
subtextStyle a = set "subtextStyle" =<< buildObj a

left ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
left a = set' "left" $ T.pixelOrPercentToForeign a

right ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
right a = set' "right" $ T.pixelOrPercentToForeign a

top ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
top a = set' "top" $ T.pixelOrPercentToForeign a

topTop ∷ ∀ m. Monad m ⇒ DSL m
topTop = set' "top" $ toForeign "top"

topMiddle ∷ ∀ m. Monad m ⇒ DSL m
topMiddle = set' "top" $ toForeign "middle"

topBottom ∷ ∀ m. Monad m ⇒ DSL m
topBottom = set' "top" $ toForeign "bottom"

bottom ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
bottom a = set' "bottom" $ T.pixelOrPercentToForeign a

bottomPx ∷ ∀ m. Monad m ⇒ Int → DSL m
bottomPx = set' "bottom" <<< toForeign

orient ∷ ∀ m. Monad m ⇒ T.Orient → DSL m
orient a = set' "orient" $ T.orientToForeign a

items ∷ ∀ f m. Monad m ⇒ F.Foldable f ⇒ f T.Item → DSL m
items a = set' "data" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

itemsDSL
  ∷ ∀ f m a
  . Monad m
  ⇒ F.Traversable f
  ⇒ f (CommandsT m a)
  → CommandsT m (f a)
itemsDSL a = do
 is ← F.for a $ buildObj
 set' "data" $ toForeign $ F.foldMap (Arr.singleton <<< snd) is
 pure $ map fst is


addItem ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addItem = set "" <=< buildObj

buildItems ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildItems = set "data" <=< buildArr

buildMarkItems ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildMarkItems is = do
  Tuple a obj ← buildArr is
  set' "data" $ toForeign obj
  pure a

calendarIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
calendarIndex i = set' "calendarIndex" $ toForeign i

visibleContent ∷ ∀ m. Monad m ⇒ Boolean → DSL m
visibleContent a = set' "showContent" $ toForeign a

showContent ∷ ∀ m. Monad m ⇒ DSL m
showContent = visibleContent true

hideContent ∷ ∀ m. Monad m ⇒ DSL m
hideContent = visibleContent false

alwaysShowContent ∷ ∀ m. Monad m ⇒ Boolean → DSL m
alwaysShowContent a = set' "alwaysShowContent" $ toForeign a

trigger ∷ ∀ m. Monad m ⇒ T.TooltipTrigger → DSL m
trigger a = set' "trigger" $ T.tooltipTriggerToForeign a

triggerOnMouseMove ∷ ∀ m. Monad m ⇒ DSL m
triggerOnMouseMove = set' "triggerOn" $ toForeign "mousemove"

triggerOnClick ∷ ∀ m. Monad m ⇒ DSL m
triggerOnClick = set' "triggerOn" $ toForeign "click"

triggerAxis ∷ ∀ m. Monad m ⇒ DSL m
triggerAxis = set' "trigger" $ toForeign "axis"

triggerItem ∷ ∀ m. Monad m ⇒ DSL m
triggerItem = set' "trigger" $ toForeign "item"

triggerEvent ∷ ∀ m. Monad m ⇒ Boolean → DSL m
triggerEvent a = set' "triggerEvent" $ toForeign a

pie ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
pie = set "pie" <=< buildObj

line ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
line = set "line" <=< buildObj

bar ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
bar = set "bar" <=< buildObj

scatter ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
scatter = set "scatter" <=< buildObj

effectScatter ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
effectScatter = set "effectScatter" <=< buildObj

treeMap ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
treeMap = set "treemap" <=< buildObj

boxPlot ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
boxPlot = set "boxplot" <=< buildObj

candlestick ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
candlestick = set "candlestick" <=< buildObj

heatMap ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
heatMap = set "heatmap" <=< buildObj

calendarSpec ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
calendarSpec = set "calendar" <=< buildObj

map_ ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
map_ = set "map" <=< buildObj

parallels ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
parallels = set "parallel" <=< buildArr

parallel ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
parallel = set "parallel" <=< buildObj

lines ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
lines = set "lines" <=< buildObj

graph ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
graph = set "graph" <=< buildObj

sankey ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
sankey = set "sankey" <=< buildObj

funnel ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
funnel = set "funnel" <=< buildObj

parallelSeries ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
parallelSeries = set "parallel" <=< buildObj

gauge ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
gauge = set "gauge" <=< buildObj

radarSeries ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
radarSeries = set "radar" <=< buildObj

xAxisIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
xAxisIndex a = set' "xAxisIndex" $ toForeign a

yAxisIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
yAxisIndex a = set' "yAxisIndex" $ toForeign a

xAxisAllIndices ∷ ∀ m. Monad m ⇒ DSL m
xAxisAllIndices = set' "xAxisIndex" $ toForeign "all"

yAxisAllIndices ∷ ∀ m. Monad m ⇒ DSL m
yAxisAllIndices = set' "yAxisIndex" $ toForeign "all"

polarIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
polarIndex a = set' "polarIndex" $ toForeign a

symbol ∷ ∀ m. Monad m ⇒ T.Symbol → DSL m
symbol a = set' "symbol" $ T.symbolToForeign a

symbolSize ∷ ∀ m. Monad m ⇒ Int → DSL m
symbolSize a = set' "symbolSize" $ toForeign a

smooth ∷ ∀ m. Monad m ⇒ Boolean → DSL m
smooth a = set' "smooth" $ toForeign a

name ∷ ∀ m. Monad m ⇒ String → DSL m
name a = set' "name" $ toForeign a

stack ∷ ∀ m. Monad m ⇒ String → DSL m
stack a = set' "stack" $ toForeign a

center ∷ ∀ m. Monad m ⇒ T.Point → DSL m
center a = set' "center" $ T.pointToForeign a

radius ∷ ∀ m. Monad m ⇒ T.Radius → DSL m
radius a = set' "radius" $ T.radiusToForeign a

singleValueRadius ∷ ∀ m. Monad m ⇒ T.SingleValueRadius → DSL m
singleValueRadius a = set' "radius" $ T.singleValueRadiusToForeign a

startAngle ∷ ∀ m. Monad m ⇒ Number → DSL m
startAngle a = set' "startAngle" $ toForeign a

axisTick ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
axisTick = set "axisTick" <=< buildObj

axisLabel ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
axisLabel = set "axisLabel" <=< buildObj

axisType ∷ ∀ m. Monad m ⇒ T.AxisType → DSL m
axisType a = set' "type" $ T.axisTypeToForeign a

value ∷ ∀ m. Monad m ⇒ Number → DSL m
value a = set' "value" $ toForeign a

values ∷ ∀ f m. Monad m ⇒ F.Foldable f ⇒ f Number → DSL m
values = set' "value" <<< toForeign <<< F.foldMap Arr.singleton

buildValues ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildValues = set "value" <=< buildArr

addValue ∷ ∀ m. Monad m ⇒ Number → DSL m
addValue = set' "" <<< toForeign

addStringValue ∷ ∀ m. Monad m ⇒ String → DSL m
addStringValue = set' "" <<< toForeign

autoValue ∷ ∀ m. Monad m ⇒ DSL m
autoValue = set' "" $ toForeign "auto"

buildNames ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildNames = set "name" <=< buildArr

addName ∷ ∀ m. Monad m ⇒ String → DSL m
addName = set' "" <<< toForeign

missingValue ∷ ∀ m. Monad m ⇒ DSL m
missingValue = set' "" undefinedValue

missingName ∷ ∀ m. Monad m ⇒ DSL m
missingName = set' "" undefinedValue

valuePair ∷ ∀ m. Monad m ⇒ String → Number → DSL m
valuePair a b = set' "value" $ toForeign [toForeign a, toForeign b]

titles ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
titles = set "title" <=< buildArr

title ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
title = set "title" <=< buildObj

text ∷ ∀ m. Monad m ⇒ String → DSL m
text a = set' "text" $ toForeign a

showDelay ∷ ∀ m. Monad m ⇒ Number → DSL m
showDelay a = set' "showDelay" $ toForeign a

hideDelay ∷ ∀ m. Monad m ⇒ Number → DSL m
hideDelay a = set' "hideDelay" $ toForeign a

pointerType ∷ ∀ m. Monad m ⇒ T.PointerType → DSL m
pointerType a = set' "type" $ T.pointerTypeToForeign a

zlevel ∷ ∀ m. Monad m ⇒ Int → DSL m
zlevel a = set' "zlevel" $ toForeign a

lineType ∷ ∀ m. Monad m ⇒ T.LineType → DSL m
lineType a = set' "type" $ toForeign a

width ∷ ∀ m. Monad m ⇒ Int → DSL m
width a = set' "width" $ toForeign a

widthPct ∷ ∀ m. Monad m ⇒ Number → DSL m
widthPct = set' "width" <<< toForeign <<< (_ <> "%") <<< show

axisPointer ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
axisPointer = set "axisPointer" <=< buildObj

scale ∷ ∀ m. Monad m ⇒ Boolean → DSL m
scale a = set' "scale" $ toForeign a

large ∷ ∀ m. Monad m ⇒ Boolean → DSL m
large a = set' "large" $ toForeign a

formatterAxis ∷ ∀ m. Monad m ⇒ (Array T.FormatterInput → String) → DSL m
formatterAxis a = set' "formatter" $ toForeign a

formatterAxisArrayValue
 ∷ ∀ m. Monad m ⇒ (Array T.FormatterInputArrayValue → String) → DSL m
formatterAxisArrayValue a = set' "formatter" $ toForeign a

formatterItem
 ∷ ∀ m. Monad m ⇒ (T.FormatterInput → String) → DSL m
formatterItem a = set' "formatter" $ toForeign a

formatterItemArrayValue
 ∷ ∀ m. Monad m ⇒ (T.FormatterInputArrayValue → String) → DSL m
formatterItemArrayValue a = set' "formatter" $ toForeign a

formatterString ∷ ∀ m. Monad m ⇒ String → DSL m
formatterString a = set' "formatter" $ toForeign a

formatterValue ∷ ∀ m. Monad m ⇒ (Number → String) → DSL m
formatterValue = set' "formatter" <<< toForeign

formatterLabel ∷ ∀ m. Monad m ⇒ (String → String) → DSL m
formatterLabel = set' "formatter" <<< toForeign

animationEnabled ∷ ∀ m. Monad m ⇒ Boolean → DSL m
animationEnabled a = set' "animation" $ toForeign a

splitLine ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
splitLine = set "splitLine" <=< buildObj

boundaryGap ∷ ∀ m. Monad m ⇒ T.Point → DSL m
boundaryGap a = set' "boundaryGap" $ T.pointToForeign a

disabledBoundaryGap ∷ ∀ m. Monad m ⇒ DSL m
disabledBoundaryGap = set' "boundaryGap" $ toForeign false

enabledBoundaryGap ∷ ∀ m. Monad m ⇒ DSL m
enabledBoundaryGap = set' "boundaryGap" $ toForeign true

hoverAnimationEnabled ∷ ∀ m. Monad m ⇒ Boolean → DSL m
hoverAnimationEnabled a = set' "hoverAnimation" $ toForeign a

showSymbol ∷ ∀ m. Monad m ⇒ Boolean → DSL m
showSymbol a = set' "showSymbol" $ toForeign a

selectedMode ∷ ∀ m. Monad m ⇒ T.SelectedMode → DSL m
selectedMode a = set' "selectedMode" $ T.selectedModeToForeign a

label ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
label = set "label" <=< buildObj

normalLabel ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normalLabel = normal

precision ∷ ∀ m. Monad m ⇒ Number → DSL m
precision = set' "precision" <<< toForeign

emphasisLabel ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT  m
emphasisLabel = emphasis

selected ∷ ∀ m. Monad m ⇒ Boolean → DSL m
selected a = set' "selected" $ toForeign a

leftPosition ∷ ∀ m. Monad m ⇒ T.HorizontalPosition → DSL m
leftPosition a = set' "left" $ T.horizontalPositionToForeign a

alignLeft ∷ ∀ m. Monad m ⇒ DSL m
alignLeft = set' "align" $ toForeign "left"

alignRight ∷ ∀ m. Monad m ⇒ DSL m
alignRight = set' "align" $ toForeign "right"

alignAuto ∷ ∀ m. Monad m ⇒ DSL m
alignAuto = set' "align" $ toForeign "auto"

funnelLeft ∷ ∀ m. Monad m ⇒ DSL m
funnelLeft = set' "funnelAlign" $ toForeign "left"

funnelRight ∷ ∀ m. Monad m ⇒ DSL m
funnelRight = set' "funnelAlign" $ toForeign "right"

funnelCenter ∷ ∀ m. Monad m ⇒ DSL m
funnelCenter = set' "funnelAlign" $ toForeign "center"

textLeft ∷ ∀ m. Monad m ⇒ DSL m
textLeft = set' "textAlign" $ toForeign "left"

textRight ∷ ∀ m. Monad m ⇒ DSL m
textRight = set' "textAlign" $ toForeign "right"

textCenter ∷ ∀ m. Monad m ⇒ DSL m
textCenter = set' "textAlign" $ toForeign "center"

textTop ∷ ∀ m. Monad m ⇒ DSL m
textTop = set' "textBaseline" $ toForeign "top"

textBottom ∷ ∀ m. Monad m ⇒ DSL m
textBottom = set' "textBaseline" $ toForeign "bottom"

textMiddle ∷ ∀ m. Monad m ⇒ DSL m
textMiddle = set' "textBaseline" $ toForeign "middle"

brush ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brush = set "brush" <=< buildObj

brushType ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brushType = set "type" <=< buildArr

brushToolbox ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brushToolbox = set "toolbox" <=< buildArr

brushModeSingle ∷ ∀ m. Monad m ⇒ DSL m
brushModeSingle = set' "brushMode" $ toForeign "single"

brushIcons ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brushIcons a = set "icon" =<< buildObj a

brushTitle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brushTitle a = set "title" =<< buildObj a

brushModeMultiple ∷ ∀ m. Monad m ⇒ DSL m
brushModeMultiple = set' "brushMode" $ toForeign "multiple"

rect ∷ ∀ m. Monad m ⇒ DSL m
rect = set' "" $ toForeign "rect"

setRect ∷ ∀ m. Monad m ⇒ String → DSL m
setRect a = set' "rect" $ toForeign a

polygon ∷ ∀ m. Monad m ⇒ DSL m
polygon = set' "" $ toForeign "polygon"

setPolygon ∷ ∀ m. Monad m ⇒ String → DSL m
setPolygon a = set' "polygon" $ toForeign a

lineX ∷ ∀ m. Monad m ⇒ DSL m
lineX = set' "" $ toForeign "lineX"

setLineX ∷ ∀ m. Monad m ⇒ String → DSL m
setLineX a = set' "lineX" $ toForeign a

lineY ∷ ∀ m. Monad m ⇒ DSL m
lineY = set' "" $ toForeign "lineY"

setLineY ∷ ∀ m. Monad m ⇒ String → DSL m
setLineY a = set' "lineY" $ toForeign a

keep ∷ ∀ m. Monad m ⇒ DSL m
keep = set' "" $ toForeign "keep"

setKeep ∷ ∀ m. Monad m ⇒ String → DSL m
setKeep a = set' "keep" $ toForeign a

clear ∷ ∀ m. Monad m ⇒ DSL m
clear = set' "" $ toForeign "clear"

setClear ∷ ∀ m. Monad m ⇒ String → DSL m
setClear a = set' "clear" $ toForeign a

dataZoom ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
dataZoom = set "dataZoom" <=< buildSeries

insideDataZoom ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
insideDataZoom = set "inside" <=< buildObj

sliderDataZoom ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
sliderDataZoom = set "slider" <=< buildObj

toolbox ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
toolbox a = set "toolbox" =<< buildObj a

feature ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
feature a = set "feature" =<< buildObj a

brushFeature ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
brushFeature a = set "brush" =<< buildObj a

magicType ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
magicType a = set "magicType" =<< buildObj a

magics ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
magics a = set "type" =<< buildArr a

magicLine ∷ ∀ m. Monad m ⇒ DSL m
magicLine = set' "" $ toForeign "line"

magicBar ∷ ∀ m. Monad m ⇒ DSL m
magicBar = set' "" $ toForeign "bar"

magicStack ∷ ∀ m. Monad m ⇒ DSL m
magicStack = set' "" $ toForeign "stack"

magicTiled ∷ ∀ m. Monad m ⇒ DSL m
magicTiled = set' "" $ toForeign "tiled"

dataView ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
dataView a = set "dataView" =<< buildObj a

dataZoomFeature ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
dataZoomFeature = set "dataZoom" <=< buildObj

splitArea ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
splitArea a = set "splitArea" =<< buildObj a

axisLine ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
axisLine a = set "axisLine" =<< buildObj a

silent ∷ ∀ m. Monad m ⇒ Boolean → DSL m
silent a = set' "silent" $ toForeign a

onZero ∷ ∀ m. Monad m ⇒ Boolean → DSL m
onZero a = set' "onZero" $ toForeign a

inverse ∷ ∀ m. Monad m ⇒ Boolean → DSL m
inverse a = set' "inverse" $ toForeign a

visualMap ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
visualMap a = set "visualMap" =<< buildSeries a

calendar ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
calendar a = set "calendar" =<< buildSeries a

continuous ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
continuous a = set "continuous" =<< buildObj a

dimension ∷ ∀ m. Monad m ⇒ Int → DSL m
dimension a = set' "dimension" $ toForeign a

textPair ∷ ∀ m. Monad m ⇒ String → String → DSL m
textPair high low = set' "text" $ toForeign [high, low]

itemHeight ∷ ∀ m. Monad m ⇒ Number → DSL m
itemHeight a = set' "itemHeight" $ toForeign a

itemWidth ∷ ∀ m. Monad m ⇒ Number → DSL m
itemWidth a = set' "itemWidth" $ toForeign a

calculable ∷ ∀ m. Monad m ⇒ Boolean → DSL m
calculable a = set' "calculable" $ toForeign a

min ∷ ∀ m. Monad m ⇒ Number → DSL m
min a = set' "min" $ toForeign a

max ∷ ∀ m. Monad m ⇒ Number → DSL m
max a = set' "max" $ toForeign a

inRange ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
inRange a = set "inRange" =<< buildObj a

outOfRange ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
outOfRange a = set "outOfRange" =<< buildObj a

controller ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
controller a = set "controller" =<< buildObj a

colorLightness ∷ ∀ m. Monad m ⇒ Number → Number → DSL m
colorLightness a b = set' "colorLightness" $ toForeign [a, b]

itemStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
itemStyle a = set "itemStyle" =<< buildObj a

normalItemStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normalItemStyle = normal

emphasisItemStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
emphasisItemStyle = emphasis

barBorderWidth ∷ ∀ m. Monad m ⇒ Number → DSL m
barBorderWidth a = set' "barBorderWidth" $ toForeign a

shadowBlur ∷ ∀ m. Monad m ⇒ Number → DSL m
shadowBlur a = set' "shadowBlur" $ toForeign a

shadowOffsetX ∷ ∀ m. Monad m ⇒ Number → DSL m
shadowOffsetX a = set' "shadowOffsetX" $ toForeign a

shadowOffsetY ∷ ∀ m. Monad m ⇒ Number → DSL m
shadowOffsetY a = set' "shadowOffsetY" $ toForeign a

shadowColor ∷ ∀ m. Monad m ⇒ C.Color → DSL m
shadowColor a = set' "shadowColor" $ toForeign $ C.cssStringRGBA a

restore ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
restore = set "restore" <=< buildObj

saveAsImage ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
saveAsImage = set "saveAsImage" <=< buildObj

z ∷ ∀ m. Monad m ⇒ Int → DSL m
z = set' "z" <<< toForeign

splitNumber ∷ ∀ m. Monad m ⇒ Int → DSL m
splitNumber = set' "splitNumber" <<< toForeign

gaugeRadius ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
gaugeRadius = set' "radius" <<< T.pixelOrPercentToForeign

detail ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
detail = set "detail" <=< buildObj

endAngle ∷ ∀ m. Monad m ⇒ Number → DSL m
endAngle = set' "endAngle" <<< toForeign

gaugePointer ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
gaugePointer = set "pointer" <=< buildObj

length ∷ ∀ m. Monad m ⇒ Int → DSL m
length = set' "length" <<< toForeign

autoColor ∷ ∀ m. Monad m ⇒ DSL m
autoColor = set' "color" $ toForeign "auto"

bolderFontWeight ∷ ∀ m. Monad m ⇒ DSL m
bolderFontWeight = set' "fontWeight" $ toForeign "bolder"

fontSize ∷ ∀ m. Monad m ⇒ Int → DSL m
fontSize = set' "fontSize" <<< toForeign

italicFontStyle ∷ ∀ m. Monad m ⇒ DSL m
italicFontStyle = set' "fontStyle" $ toForeign "italic"

offsetCenter ∷ ∀ m. Monad m ⇒ T.Point → DSL m
offsetCenter = set' "offsetCenter" <<< T.pointToForeign

subtext ∷ ∀ m. Monad m ⇒ String → DSL m
subtext = set' "subtext" <<< toForeign

readOnly ∷ ∀ m. Monad m ⇒ Boolean → DSL m
readOnly = set' "readOnly" <<< toForeign

positionInside ∷ ∀ m. Monad m ⇒ DSL m
positionInside = set' "position" $ toForeign "inside"

positionTop ∷ ∀ m. Monad m ⇒ DSL m
positionTop = set' "position" $ toForeign "top"

labelLine ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
labelLine = set "labelLine" <=< buildObj

normalLabelLine ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normalLabelLine = normal

emphasisLabelLine
 ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
emphasisLabelLine = emphasis

opacity ∷ ∀ m. Monad m ⇒ Number → DSL m
opacity = set' "opacity" <<< toForeign

maxSize ∷ ∀ m. Monad m ⇒ Int → DSL m
maxSize = set' "maxSize" <<< toForeign

maxSizePct ∷ ∀ m. Monad m ⇒ Number → DSL m
maxSizePct = set' "maxSize" <<< toForeign <<< (_ <> "%") <<< show

borderColor ∷ ∀ m. Monad m ⇒ C.Color → DSL m
borderColor = set' "borderColor" <<< toForeign <<< C.toHexString

borderWidth ∷ ∀ m. Monad m ⇒ Int → DSL m
borderWidth = set' "borderWidth" <<< toForeign

normalLineStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normalLineStyle = normal

emphasisLineStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
emphasisLineStyle = emphasis

leftCenter ∷ ∀ m. Monad m ⇒ DSL m
leftCenter = set' "left" $ toForeign "center"

leftLeft ∷ ∀ m. Monad m ⇒ DSL m
leftLeft = set' "left" $ toForeign "left"

leftRight ∷ ∀ m. Monad m ⇒ DSL m
leftRight = set' "left" $ toForeign "right"

itemGap ∷ ∀ m. Monad m ⇒ Int → DSL m
itemGap = set' "itemGap" <<< toForeign

indicators ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
indicators = set "indicator" <=< buildArr

indicator ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
indicator = set "" <=< buildObj

radarName ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
radarName = set "name" <=< buildObj

nameGap ∷ ∀ m. Monad m ⇒ Number → DSL m
nameGap = set' "nameGap" <<< toForeign

polygonShape ∷ ∀ m. Monad m ⇒ DSL m
polygonShape = set' "shape" $ toForeign "polygon"

circleShape ∷ ∀ m. Monad m ⇒ DSL m
circleShape = set' "shape" $ toForeign "circle"

lineStylePair ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
lineStylePair = lineStyle

areaStylePair ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
areaStylePair = areaStyle

normalAreaStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normalAreaStyle = normal

emphasisAreaStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
emphasisAreaStyle = emphasis

radars ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
radars = set "radar" <=< buildArr

radar ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
radar = set "radar" <=< buildObj

ascending ∷ ∀ m. Monad m ⇒ DSL m
ascending = set' "sort" $ toForeign "ascending"

descending ∷ ∀ m. Monad m ⇒ DSL m
descending = set' "sort" $ toForeign "descending"

animationDurationUpdate ∷ ∀ m. Monad m ⇒ Int → DSL m
animationDurationUpdate = set' "animationDurationUpdate" <<< toForeign

animationEasingUpdateQuinticInOut ∷ ∀ m. Monad m ⇒ DSL m
animationEasingUpdateQuinticInOut = set' "animationEasingUpdate" $ toForeign "quinticInOut"

roam ∷ ∀ m. Monad m ⇒ Boolean → DSL m
roam = set' "roam" <<< toForeign

edgeSymbols ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
edgeSymbols = set "edgeSymbol" <=< buildArr

circleEdgeSymbol ∷ ∀ m. Monad m ⇒ DSL m
circleEdgeSymbol = set' "" $ toForeign "circle"

arrowEdgeSymbol ∷ ∀ m. Monad m ⇒ DSL m
arrowEdgeSymbol = set' "" $ toForeign "arrow"

edgeSymbolSize ∷ ∀ m. Monad m ⇒ Int → DSL m
edgeSymbolSize = set' "edgeSymbolSize" <<< toForeign

edgeSymbolSizes ∷ ∀ m. Monad m ⇒ Int → Int → DSL m
edgeSymbolSizes a b = set' "edgeSymbolSize" $ toForeign [a, b]

buildLinks ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildLinks = set "links" <=< buildArr

addLink ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addLink = set "" <=< buildObj

links ∷ ∀ m. Monad m ⇒ Array { source ∷ String, target ∷ String } → DSL  m
links = set' "links" <<< toForeign

edgeLabel ∷ ∀ m. Monad m ⇒ CommandsT  m ~> CommandsT m
edgeLabel = set "edgeLabel" <=< buildObj

normalEdgeLabel
  ∷ ∀ m
  . Monad m
  ⇒ CommandsT m
  ~> CommandsT m
normalEdgeLabel = normal

emphasisEdgeLabel
 ∷ ∀ m
 . Monad m
 ⇒ CommandsT m
 ~> CommandsT m
emphasisEdgeLabel = emphasis

x ∷ ∀ m. Monad m ⇒ Number → DSL m
x = set' "x" <<< toForeign

y ∷ ∀ m. Monad m ⇒ Number → DSL m
y = set' "y" <<< toForeign

curveness ∷ ∀ m. Monad m ⇒ Number → DSL m
curveness = set' "curveness" <<< toForeign

symbolSizes ∷ ∀ m. Monad m ⇒ Int → Int → DSL m
symbolSizes a b = set' "symbolSize" $ toForeign [a, b]

symbolSizeArrFunc ∷ ∀ m. Monad m ⇒ (Array Number → Number) → DSL m
symbolSizeArrFunc fn = set' "symbolSize" $ toForeign fn

sourceIx ∷ ∀ m. Monad m ⇒ Int → DSL m
sourceIx = set' "source" <<< toForeign

targetIx ∷ ∀ m. Monad m ⇒ Int → DSL m
targetIx = set' "target" <<< toForeign

sourceName ∷ ∀ m. Monad m ⇒ String → DSL m
sourceName = set' "source" <<< toForeign

targetName ∷ ∀ m. Monad m ⇒ String → DSL m
targetName = set' "target" <<< toForeign

subtargetName ∷ ∀ m. Monad m ⇒ String → DSL m
subtargetName = set' "subtarget" <<< toForeign

layoutNone ∷ ∀ m. Monad m ⇒ DSL m
layoutNone = set' "layout" $ toForeign "none"

layoutCircular ∷ ∀ m. Monad m ⇒ DSL m
layoutCircular = set' "layout" $ toForeign "circular"

layoutForce ∷ ∀ m. Monad m ⇒ DSL m
layoutForce = set' "layout" $ toForeign "force"

missingSeries ∷ ∀ m. Monad m ⇒ DSL m
missingSeries = set' "" undefinedValue

missingItem ∷ ∀ m. Monad m ⇒ DSL m
missingItem = set' "" undefinedValue

rotate ∷ ∀ m. Monad m ⇒ Number → DSL m
rotate = set' "rotate" <<< toForeign

fontFamily ∷ ∀ m. Monad m ⇒ String → DSL m
fontFamily = set' "fontFamily" <<< toForeign

addParallelAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addParallelAxis = set "" <=< buildObj

parallelAxes ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
parallelAxes = set "parallelAxis" <=< buildArr

parallelAxisDefault ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
parallelAxisDefault = set "parallelAxisDefault" <=< buildObj

yAxes ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
yAxes = set "yAxis" <=< buildArr

xAxes ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
xAxes = set "xAxis" <=< buildArr

addYAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addYAxis = set "" <=< buildObj

addXAxis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addXAxis = set "" <=< buildObj

interval ∷ ∀ m. Monad m ⇒ Int → DSL m
interval = set' "interval" <<< toForeign

lineAxisPointer ∷ ∀ m. Monad m ⇒ DSL m
lineAxisPointer = set' "type" $ toForeign "line"

crossAxisPointer ∷ ∀ m. Monad m ⇒ DSL m
crossAxisPointer = set' "type" $ toForeign "cross"

solidLine ∷ ∀ m. Monad m ⇒ DSL m
solidLine = set' "type" $ toForeign "solid"

dashedLine ∷ ∀ m. Monad m ⇒ DSL m
dashedLine = set' "type" $ toForeign "dashed"

dottedLine ∷ ∀ m. Monad m ⇒ DSL m
dottedLine = set' "type" $ toForeign "dotted"

widthNum ∷ ∀ m. Monad m ⇒ Number → DSL m
widthNum = set' "width" <<< toForeign

crossStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
crossStyle = set "crossStyle" <=< buildObj

normal ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
normal = set "normal" <=< buildObj

lineStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
lineStyle = set "lineStyle" <=< buildObj

areaStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
areaStyle = set "areaStyle" <=< buildObj

emphasis ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
emphasis = set "emphasis" <=< buildObj

heightPixelOrPercent ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
heightPixelOrPercent = set' "height" <<< T.pixelOrPercentToForeign

heightPct ∷ ∀ m. Monad m ⇒ Number → DSL m
heightPct = set' "height" <<< toForeign <<< (_ <> "%") <<< show

widthPixelOrPercent ∷ ∀ m. Monad m ⇒ T.PixelOrPercent → DSL m
widthPixelOrPercent = set' "width" <<< T.pixelOrPercentToForeign

padding ∷ ∀ m. Monad m ⇒ Number → DSL m
padding = set' "padding" <<< toForeign

enterable ∷ ∀ m. Monad m ⇒ Boolean → DSL m
enterable = set' "enterable" <<< toForeign

transitionDuration ∷ ∀ m. Monad m ⇒ Number → DSL m
transitionDuration = set' "transitionDuration" <<< toForeign

extraCssText ∷ ∀ m. Monad m ⇒ String → DSL m
extraCssText = set' "extraCssText" <<< toForeign

gridIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
gridIndex a = set' "gridIndex" $ toForeign a

radarIndex ∷ ∀ m. Monad m ⇒ Number → DSL m
radarIndex = set' "radarIndex" <<< toForeign

parallelIndex ∷ ∀ m. Monad m ⇒ Int → DSL m
parallelIndex = set' "parallelIndex" <<< toForeign

treeMapNodeId ∷ ∀ m. Monad m ⇒ String → DSL m
treeMapNodeId = set' "id" <<< toForeign

visualDimension ∷ ∀ m. Monad m ⇒ Int → DSL m
visualDimension = set' "visualDimension" <<< toForeign

visibleMin ∷ ∀ m. Monad m ⇒ Number → DSL m
visibleMin = set' "visibleMin" <<< toForeign

childVisibleMin ∷ ∀ m. Monad m ⇒ Number → DSL m
childVisibleMin = set' "childVisibleMin" <<< toForeign

category ∷ ∀ m. Monad m ⇒ Int → DSL m
category = set' "category" <<< toForeign

coords ∷ ∀ f m. Monad m ⇒ F.Foldable f ⇒ f T.Coord → DSL m
coords a = set' "coords" $ toForeign $ F.foldMap (Arr.singleton <<< toForeign) a

valueIndex ∷ ∀ m. Monad m ⇒ Number → DSL m
valueIndex = set' "valueIndex" <<< toForeign

valueDim ∷ ∀ m. Monad m ⇒ String → DSL m
valueDim = set' "valueDim" <<< toForeign

markType ∷ ∀ m. Monad m ⇒ String → DSL m
markType = set' "type" <<< toForeign

margin ∷ ∀ m. Monad m ⇒ Int → DSL m
margin = set' "margin" <<< toForeign

markLine ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
markLine = set "markLine" <=< buildObj

markPoint ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
markPoint = set "markPoint" <=< buildObj

markArea ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
markArea = set "markArea" <=< buildObj

repulsion ∷ ∀ m. Monad m ⇒ Number → DSL m
repulsion = set' "repulsion" <<< toForeign

gravity ∷ ∀ m. Monad m ⇒ Number → DSL m
gravity = set' "gravity" <<< toForeign

edgeLength ∷ ∀ m. Monad m ⇒ Number → DSL m
edgeLength = set' "edgeLength" <<< toForeign

edgeLengths ∷ ∀ m. Monad m ⇒ Number → Number → DSL m
edgeLengths a b = set' "edgeLength" $ toForeign [ a, b ]

layoutAnimation ∷ ∀ m. Monad m ⇒ Boolean → DSL m
layoutAnimation = set' "layoutAnimation" <<< toForeign

circular ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
circular = set "circular" <=< buildObj

rotateLabel ∷ ∀ m. Monad m ⇒ Boolean → DSL m
rotateLabel = set' "rotateLabel" <<< toForeign

force ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
force = set "force" <=< buildObj

buildCategories ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildCategories = set "categories" <=< buildArr

addCategory ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
addCategory = set "" <=< buildObj

colorSource ∷ ∀ m. Monad m ⇒ DSL m
colorSource = set' "color" $ toForeign "source"

colorTarget ∷ ∀ m. Monad m ⇒ DSL m
colorTarget = set' "target" $ toForeign "target"

buildCoord ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildCoord dsl = do
  Tuple a xx ← get "x" dsl
  Tuple _ yy ← get "y" dsl
  set' "coord" $ toForeign $ Arr.catMaybes [ xx, yy ]
  pure a

buildCenter ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildCenter dsl = do
  Tuple a xx ← get "x" dsl
  Tuple _ yy ← get "y" dsl
  set' "center" $ toForeign $ Arr.catMaybes [ xx, yy ]
  pure a

buildRadius ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildRadius dsl = do
  Tuple a s ← get "start" dsl
  Tuple _ e ← get "end" dsl
  set' "radius" $ toForeign $ Arr.concat [s, e]
  pure a

setStart ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setStart dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "start"
  pure a

setEnd ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setEnd dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "end"
  pure a

setBarWidth ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setBarWidth dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "barWidth"
  pure a

setX ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setX dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "x"
  pure a

setY ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setY dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "y"
  pure a

setZ ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setZ dsl = do
  Tuple a keys  ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "z"
  pure a

coordXIx ∷ ∀ m. Monad m ⇒ Int → DSL m
coordXIx = set' "x" <<< toForeign

coordXValue ∷ ∀ m. Monad m ⇒ String → DSL m
coordXValue = set' "x" <<< toForeign

coordY ∷ ∀ m. Monad m ⇒ String → DSL m
coordY = set' "y" <<< toForeign

pixels ∷ ∀ m. Monad m ⇒ Int → DSL m
pixels = set' "pixels" <<< toForeign

percents ∷ ∀ m. Monad m ⇒ Number → DSL m
percents = set' "percents" <<< toForeign <<< (_ <> "%") <<< show

setWidth ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
setWidth dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "width"
  pure a

buildGaugeRadius ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildGaugeRadius dsl = do
  Tuple a keys ← lastWithKeys ["pixels", "percents"] dsl
  F.for_ (keys ∷ Array Foreign) $ set' "radius"
  pure a

buildOffsetCenter ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildOffsetCenter dsl = do
  Tuple a xx ← get "x" dsl
  Tuple _ yy ← get "y" dsl
  set' "offsetCenter" $ toForeign $ Arr.catMaybes [ xx, yy ]
  pure a

containLabel ∷ ∀ m. Monad m ⇒ Boolean → DSL m
containLabel = set' "containLabel" <<< toForeign

polarCoordinateSystem ∷ ∀ m. Monad m ⇒ DSL m
polarCoordinateSystem = set' "coordinateSystem" $ toForeign "polar"

cartesianCoordinateSystem ∷ ∀ m. Monad m ⇒ DSL m
cartesianCoordinateSystem = set' "coordinateSystem" $ toForeign "cartesian2d"

geoCoordinateSystem ∷ ∀ m. Monad m ⇒ DSL m
geoCoordinateSystem = set' "coordinateSystem" $ toForeign "geo"

calendarCoordinateSystem ∷ ∀ m. Monad m ⇒ DSL m
calendarCoordinateSystem = set' "coordinateSystem" $ toForeign "calendar"

dim ∷ ∀ m. Monad m ⇒ Int → DSL m
dim = set' "dim" <<< toForeign

nameLocationStart ∷ ∀ m. Monad m ⇒ DSL m
nameLocationStart = set' "nameLocation" $ toForeign "start"

nameLocationEnd ∷ ∀ m. Monad m ⇒ DSL m
nameLocationEnd = set' "nameLocation" $ toForeign "end"

nameLocationMiddle ∷ ∀ m. Monad m ⇒ DSL m
nameLocationMiddle = set' "nameLocation" $ toForeign "middle"

nameTextStyle ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
nameTextStyle o = set "nameTextStyle" =<< buildObj o

nameRotate ∷ ∀ m. Monad m ⇒ Number → DSL m
nameRotate o = set' "nameRotate" $ toForeign o

buildCellSize ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildCellSize = set "cellSize" <=< buildArr

buildRange ∷ ∀ m. Monad m ⇒ CommandsT m ~> CommandsT m
buildRange = set "range" <=< buildArr

addDateValue ∷ ∀ m. Monad m ⇒ Date → DSL m
addDateValue dt =
 set' "" <<< toForeign
   $ year' dt
   <> "-"
   <> month' dt
   <> "-"
   <> day' dt
 where
 year' = show <<< fromEnum <<< year
 month' = show <<< fromEnum <<< month
 day' = show <<< fromEnum <<< day

useUTC ∷ ∀ m. Monad m ⇒ Boolean → DSL m
useUTC = set' "useUTC" <<< toForeign
