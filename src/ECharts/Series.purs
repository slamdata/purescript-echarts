module ECharts.Series (
  setSeries,
  Series(..),
  UniversalSeriesRec(),
  LineSeriesRec(),
  BarSeriesRec(),
  ScatterSeriesRec(),
  PieSeriesRec(),
  RadarSeriesRec(),
  ChordSeriesRec(),
  ForceSeriesRec(),
  CandlestickSeriesRec(),
  MapSeriesRec(),
  GaugeSeriesRec(),
  FunnelSeriesRec(),
  EventRiverSeriesRec(),
  universalSeriesDefault,
  lineSeriesDefault,
  barSeriesDefault,
  scatterSeriesDefault,
  pieSeriesDefault,
  radarSeriesDefault,
  chordSeriesDefault,
  forceSeriesDefault,
  candlestickSeriesDefault,
  mapSeriesDefault,
  gaugeSeriesDefault,
  funnelSeriesDefault,
  eventRiverSeriesDefault
  ) where 

import Prelude
import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Data.Either
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Tuple (Tuple(..))
import Data.StrMap hiding (toList)
import Data.Array (concat)
import Data.List (toList)

import ECharts.Common
import ECharts.Coords
import ECharts.Chart
import ECharts.Tooltip
import ECharts.Style.Item
import ECharts.Mark.Line
import ECharts.Mark.Point
import ECharts.Item.Data
import ECharts.Symbol
import ECharts.Series.Force
import ECharts.Series.Gauge
import ECharts.Series.EventRiver
import ECharts.Axis
import ECharts.Title
import ECharts.Utils


data ChartType = Line | Bar | Scatter | Candlestick | Pie | Radar
               | Chord | Force | Map | Gauge | Funnel | EventRiver

instance chartTypeEncodeJson :: EncodeJson ChartType where
  encodeJson a = fromString $ case a of 
    Line -> "line"
    Bar -> "bar"
    Scatter -> "scatter"
    Candlestick -> "k"
    Pie -> "pie"
    Radar -> "radar"
    Chord -> "chord"
    Force -> "force"
    Map -> "map"
    Gauge -> "gauge"
    Funnel -> "funnel"
    EventRiver -> "eventRiver"

data Series = LineSeries
              {common :: UniversalSeriesRec, lineSeries :: LineSeriesRec}
            | BarSeries
              {common :: UniversalSeriesRec, barSeries ::  BarSeriesRec}
            | ScatterSeries
              {common :: UniversalSeriesRec, scatterSeries :: ScatterSeriesRec}
            | CandlestickSeries 
              {common :: UniversalSeriesRec, candlestickSeries :: CandlestickSeriesRec}
            | PieSeries
              {common :: UniversalSeriesRec, pieSeries ::  PieSeriesRec}
            | RadarSeries
              {common :: UniversalSeriesRec, radarSeries :: RadarSeriesRec}
            | ChordSeries
              {common :: UniversalSeriesRec, chordSeries :: ChordSeriesRec}
            | ForceSeries
              {common :: UniversalSeriesRec, forceSeries:: ForceSeriesRec}
            | MapSeries
              {common :: UniversalSeriesRec, mapSeries :: MapSeriesRec}
            | GaugeSeries
              {common :: UniversalSeriesRec, gaugeSeries :: GaugeSeriesRec}
            | FunnelSeries
              {common :: UniversalSeriesRec, funnelSeries :: FunnelSeriesRec}
            | EventRiverSeries
              {common :: UniversalSeriesRec, eventRiverSeries :: EventRiverSeriesRec}

typeForSeries :: Series -> Array JAssoc
typeForSeries series =
  ["type" := getType series] where
    getType s = case s of
      LineSeries _ -> Line
      BarSeries _ -> Bar
      CandlestickSeries _ -> Candlestick
      ScatterSeries _ -> Scatter
      PieSeries _ -> Pie
      RadarSeries _ -> Radar
      ChordSeries _ -> Chord
      ForceSeries _ -> Force
      MapSeries _ -> Map
      GaugeSeries _ -> Gauge
      FunnelSeries _ -> Funnel
      EventRiverSeries _ -> EventRiver



type UniversalSeriesRec = {
  name :: Maybe String,
  tooltip :: Maybe Tooltip,
  clickable :: Maybe Boolean,
  itemStyle :: Maybe ItemStyle,
  markPoint :: Maybe MarkPoint,
  markLine :: Maybe MarkLine
  }
universalSeriesDefault :: UniversalSeriesRec
universalSeriesDefault = {
  name: Nothing,
  tooltip: Nothing,
  clickable: Nothing,
  itemStyle: Nothing,
  markPoint: Nothing,
  markLine: Nothing
  }
universalRecEncode :: UniversalSeriesRec -> Array JAssoc
universalRecEncode r =
   [
    "name" := r.name,
    "tooltip" := r.tooltip,
    "clickable" := r.clickable,
    "itemStyle" := r.itemStyle,
    "markPoint" := r.markPoint,
    "markLine" := r.markLine
    ]
universalForSeries :: Series -> Array JAssoc
universalForSeries series =
  universalRecEncode $ case series of
    LineSeries {common = u} -> u
    BarSeries {common = u} -> u
    CandlestickSeries {common = u} -> u
    ScatterSeries {common = u} -> u
    PieSeries {common = u} -> u
    RadarSeries {common = u} -> u
    ChordSeries {common = u} -> u
    ForceSeries {common = u} -> u
    MapSeries {common = u} -> u
    GaugeSeries {common = u} -> u
    FunnelSeries {common = u} -> u
    EventRiverSeries {common = u} -> u



type LineSeriesRec = {
  "data" ::Maybe (Array ItemData),
  stack :: Maybe String,
  xAxisIndex :: Maybe Number,
  yAxisIndex :: Maybe Number,
  symbol :: Maybe Symbol,
  symbolSize :: Maybe SymbolSize,
  symbolRotate :: Maybe Boolean,
  showAllSymbol :: Maybe Boolean,
  smooth :: Maybe Boolean,
  legendHoverLink :: Maybe Boolean
  }
lineSeriesDefault :: LineSeriesRec
lineSeriesDefault  = {
  "data": Nothing,
  stack: Nothing,
  xAxisIndex: Nothing,
  yAxisIndex: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  symbolRotate: Nothing,
  showAllSymbol: Nothing,
  smooth: Nothing,
  legendHoverLink: Nothing
  }
lineRecEncode :: LineSeriesRec -> (Array JAssoc)
lineRecEncode r = [
  "data" := r.data,
  "stack" := r.stack,
  "xAxisIndex" := r.xAxisIndex,
  "yAxisIndex" := r.yAxisIndex,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "symbolRotate" := r.symbolRotate,
  "showAllSymbol" := r.showAllSymbol,
  "smooth" := r.smooth,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeLineRec :: JObject -> Either String LineSeriesRec
decodeLineRec obj =
  { "data": _
  , stack: _
  , xAxisIndex: _
  , yAxisIndex: _
  , symbol: _
  , symbolSize: _
  , symbolRotate: _
  , showAllSymbol: _
  , smooth: _
  , legendHoverLink: _ } <$>
  (obj .? "data") <*>
  (obj .? "stack") <*>
  (obj .? "xAxisIndex") <*>
  (obj .? "yAxisIndex") <*>
  (obj .? "symbol") <*>
  (obj .? "symbolSize") <*>
  (obj .? "symbolRotate") <*>
  (obj .? "showAllSymbol") <*>
  (obj .? "smooth") <*>
  (obj .? "legendHoverLink")

type BarSeriesRec = {
  "data" ::Maybe (Array ItemData),
  stack :: Maybe String,
  xAxisIndex :: Maybe Number,
  yAxisIndex :: Maybe Number,
  barGap :: Maybe PercentOrPixel,
  barCategoryGap :: Maybe PercentOrPixel,
  barMinHeight :: Maybe Number,
  barWidth :: Maybe Number,
  barMaxWidth :: Maybe Number,
  legendHoverLink :: Maybe Boolean
  }
barSeriesDefault :: BarSeriesRec
barSeriesDefault = {
  "data": Nothing,
  stack: Nothing,
  xAxisIndex: Nothing,
  yAxisIndex: Nothing,
  barGap: Nothing,
  barCategoryGap: Nothing,
  barMinHeight: Nothing,
  barWidth: Nothing,
  barMaxWidth: Nothing,
  legendHoverLink: Nothing
  }
barRecEncode :: BarSeriesRec -> Array JAssoc
barRecEncode r = [
  "data" := r.data,
  "stack" := r.stack,
  "xAxisIndex" := r.xAxisIndex,
  "yAxisIndex" := r.yAxisIndex,
  "barGap" := r.barGap,
  "barCategoryGap" := r.barCategoryGap,
  "barMinHeight" := r.barMinHeight,
  "barWidth" := r.barWidth,
  "barMaxWidth" := r.barMaxWidth,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeBarRec :: JObject -> Either String BarSeriesRec
decodeBarRec obj =
  { "data": _
  , stack: _
  , xAxisIndex: _
  , yAxisIndex: _
  , barGap: _
  , barCategoryGap: _
  , barMinHeight: _
  , barWidth: _
  , barMaxWidth: _
  , legendHoverLink: _ } <$>
  (obj .? "data") <*>
  (obj .? "stack") <*>
  (obj .? "xAxisIndex") <*>
  (obj .? "yAxisIndex") <*>
  (obj .? "barGap") <*>
  (obj .? "barCategoryGap") <*>
  (obj .? "barMinHeight") <*>
  (obj .? "barWidth") <*>
  (obj .? "barMaxWidth") <*>
  (obj .? "legendHoverLink")


type ScatterSeriesRec = {
  "data" ::Maybe (Array ItemData),
  xAxisIndex :: Maybe Number,
  yAxisIndex :: Maybe Number,
  symbol :: Maybe Symbol,
  symbolSize :: Maybe SymbolSize,
  symbolRotate :: Maybe Boolean,
  large :: Maybe Boolean,
  largeThreshold :: Maybe Number,
  legendHoverLink :: Maybe Boolean
  }
scatterSeriesDefault :: ScatterSeriesRec
scatterSeriesDefault = {
  "data": Nothing,
  xAxisIndex: Nothing,
  yAxisIndex: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  symbolRotate: Nothing,
  large: Nothing,
  largeThreshold: Nothing,
  legendHoverLink: Nothing
  }

scatterRecEncode :: ScatterSeriesRec -> Array JAssoc
scatterRecEncode r = [
  "data" := r.data,
  "xAxisIndex" := r.xAxisIndex,
  "yAxisIndex" := r.yAxisIndex,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "symbolRotate" := r.symbolRotate,
  "large" := r.large,
  "largeThreshold" := r.largeThreshold,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeScatterSeriesRec :: JObject -> Either String ScatterSeriesRec
decodeScatterSeriesRec o =
  { "data": _
  , xAxisIndex: _
  , yAxisIndex: _
  , symbol: _
  , symbolSize: _
  , symbolRotate: _
  , large: _
  , largeThreshold: _
  , legendHoverLink: _ } <$>
  (o .? "data") <*>
  (o .? "xAxisIndex") <*>
  (o .? "yAxisIndex") <*>
  (o .? "symbol") <*>
  (o .? "symbolSize") <*>
  (o .? "symbolRotate") <*>
  (o .? "large") <*>
  (o .? "largeThreshold") <*>
  (o .? "legendHoverLink") 
    


type CandlestickSeriesRec = {
  "data" ::Maybe (Array ItemData),
  xAxisIndex :: Maybe Number,
  yAxisIndex :: Maybe Number,
  barMinHeight :: Maybe Number,
  barWidth :: Maybe Number,
  barMaxWidth :: Maybe Number
  }
candlestickSeriesDefault :: CandlestickSeriesRec
candlestickSeriesDefault = {
  "data": Nothing,
  xAxisIndex: Nothing,
  yAxisIndex: Nothing,
  barMinHeight: Nothing,
  barWidth: Nothing,
  barMaxWidth: Nothing
  }
candlestickRecEncode :: CandlestickSeriesRec -> Array JAssoc
candlestickRecEncode r = [
  "data" := r.data,
  "xAxisIndex" := r.xAxisIndex,
  "yAxisIndex" := r.yAxisIndex,
  "barMinHeight" := r.barMinHeight,
  "barWidth" := r.barWidth,
  "barMaxWidth" := r.barMaxWidth
  ]

decodeCandleStickSeries :: JObject -> Either String CandlestickSeriesRec
decodeCandleStickSeries o =
  { "data": _
  , xAxisIndex: _
  , yAxisIndex: _
  , barMinHeight: _
  , barWidth: _
  , barMaxWidth: _ } <$>
  (o .? "data") <*>
  (o .? "xAxisIndex") <*>
  (o .? "yAxisIndex") <*>
  (o .? "barMinHeight") <*>
  (o .? "barWidth") <*>
  (o .? "barMaxWidth")
                                 
type PieSeriesRec = {
  "data" ::Maybe (Array ItemData),
  center :: Maybe Center,
  radius :: Maybe Radius,
  startAngle :: Maybe Number,
  minAngle :: Maybe Number,
  clockWise :: Maybe Boolean,
  roseType :: Maybe RoseType,
  selectedOffset :: Maybe Number,
  selectedMode :: Maybe SelectedMode,
  legendHoverLink :: Maybe Boolean
  }
pieSeriesDefault :: PieSeriesRec
pieSeriesDefault = {
  "data": Nothing,
  center: Nothing,
  radius: Nothing,
  startAngle: Nothing,
  minAngle: Nothing,
  clockWise: Nothing,
  roseType: Nothing,
  selectedOffset: Nothing,
  selectedMode: Nothing,
  legendHoverLink: Nothing
  }

pieRecEncode :: PieSeriesRec -> Array JAssoc
pieRecEncode r = [
  "data" := r.data,
  "center" := r.center,
  "radius" := r.radius,
  "startAngle" := r.startAngle,
  "minAngle" := r.minAngle,
  "clockWise" := r.clockWise,
  "roseType" := r.roseType,
  "selectedOffset" := r.selectedOffset,
  "selectedMode" := r.selectedMode,
  "legendHoverLink" := r.legendHoverLink
  ]

decodePieSeriesRec :: JObject -> Either String PieSeriesRec
decodePieSeriesRec o =
  { "data": _
  , center: _
  , radius: _
  , startAngle: _
  , minAngle: _
  , clockWise: _
  , roseType: _
  , selectedOffset: _
  , selectedMode: _
  , legendHoverLink: _ } <$>
  (o .? "data") <*>
  (o .? "center") <*>
  (o .? "radius") <*>
  (o .? "startAngle") <*>
  (o .? "minAngle") <*>
  (o .? "clockWise") <*>
  (o .? "roseType") <*>
  (o .? "selectedOffset") <*>
  (o .? "selectedMode") <*>
  (o .? "legendHoverLink")

                 
type RadarSeriesRec = {
  "data" ::Maybe (Array ItemData),

  polarIndex :: Maybe Number,
  symbol :: Maybe Symbol,
  symbolSize :: Maybe SymbolSize,
  symbolRotate :: Maybe Boolean,
  legendHoverLink :: Maybe Boolean
  }

radarSeriesDefault :: RadarSeriesRec
radarSeriesDefault = {
  "data": Nothing,
  polarIndex: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  symbolRotate: Nothing,
  legendHoverLink: Nothing
  }
radarRecEncode :: RadarSeriesRec -> Array JAssoc
radarRecEncode r = [
  "data" := r.data,
  "polarIndex" := r.polarIndex,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "symbolRotate" := r.symbolRotate,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeRadarSeriesRec :: JObject -> Either String RadarSeriesRec
decodeRadarSeriesRec o =
  { "data": _
  , polarIndex: _
  , symbol: _
  , symbolSize: _
  , symbolRotate: _
  , legendHoverLink: _ } <$>
  (o .? "data") <*>
  (o .? "polarIndex") <*>
  (o .? "symbol" ) <*>
  (o .? "symbolSize") <*>
  (o .? "symbolRotate") <*>
  (o .? "legendHoverLink") 

type ChordSeriesRec = {
  nodes :: Maybe (Array Node),
  categories :: Maybe (Array ForceCategory),
  links :: Maybe (Array Link),
  matrix :: Maybe Matrix,

  "data" ::Maybe (Array ItemData),
  ribbonType :: Maybe Boolean,
  symbol :: Maybe Symbol,
  symbolSize :: Maybe SymbolSize,
  minRadius :: Maybe Number,
  maxRadius :: Maybe Number,
  showScale :: Maybe Boolean,
  showScaleText :: Maybe Boolean,
  padding :: Maybe Number,
  sort :: Maybe Sort,
  sortSub :: Maybe Sort,
  clockWise :: Maybe Boolean
  }
chordSeriesDefault :: ChordSeriesRec
chordSeriesDefault = {
  nodes: Nothing,
  categories: Nothing,
  links: Nothing,
  matrix: Nothing,
  "data": Nothing,
  ribbonType: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  minRadius: Nothing,
  maxRadius: Nothing,
  showScale: Nothing,
  showScaleText: Nothing,
  padding: Nothing,
  sort: Nothing,
  sortSub: Nothing,
  clockWise: Nothing
  }
chordRecEncode :: ChordSeriesRec -> Array JAssoc
chordRecEncode r = [
  "nodes" := r.nodes,
  "categories" := r.categories,
  "links" := r.links,
  "matrix" := r.matrix,
  "data" := r.data,
  "ribbonType" := r.ribbonType,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "minRadius" := r.minRadius,
  "maxRadius" := r.maxRadius,
  "showScale" := r.showScale,
  "showScaleText" := r.showScaleText,
  "padding" := r.padding,
  "sort" := r.sort,
  "sortSub" := r.sortSub,
  "clockWise" := r.clockWise
  ]

decodeChordSeriesRec :: JObject -> Either String ChordSeriesRec
decodeChordSeriesRec o =
  { nodes: _
  , categories: _
  , links: _
  , matrix: _
  , "data": _
  , ribbonType: _
  , symbol: _
  , symbolSize: _
  , minRadius: _
  , maxRadius: _
  , showScale: _
  , showScaleText: _
  , padding: _
  , sort: _
  , sortSub: _
  , clockWise: _ } <$>
  (o .? "nodes") <*>
  (o .? "categories") <*>
  (o .? "links") <*>
  (o .? "matrix") <*>
  (o .? "data") <*>
  (o .? "ribbonType") <*>
  (o .? "symbol") <*>
  (o .? "symbolSize") <*>
  (o .? "minRadius") <*>
  (o .? "maxRadius") <*>
  (o .? "showScale") <*>
  (o .? "showScaleText") <*>
  (o .? "padding") <*>
  (o .? "sort") <*>
  (o .? "sortSub") <*>
  (o .? "clockWise")
  

type ForceSeriesRec = {
  categories :: Maybe (Array ForceCategory),
  nodes :: Maybe (Array Node),
  links :: Maybe (Array Link),
  matrix :: Maybe Matrix,

  center :: Maybe Center,
  size :: Maybe Number,
  minRadius :: Maybe Number,
  maxRadius :: Maybe Number,
  symbol :: Maybe Symbol,
  symbolSize :: Maybe SymbolSize,
  linkSymbol :: Maybe Symbol,
  linkSymbolSize :: Maybe Symbol,
  scaling :: Maybe Number,
  gravity :: Maybe Number,
  draggable :: Maybe Number,
  large :: Maybe Boolean,
  useWorker :: Maybe Boolean,
  steps :: Maybe Number,
  ribbonType :: Maybe Boolean
  }
forceSeriesDefault :: ForceSeriesRec
forceSeriesDefault = {
  categories: Nothing,
  nodes: Nothing,
  links: Nothing,
  matrix: Nothing,
  center: Nothing,
  size: Nothing,
  minRadius: Nothing,
  maxRadius: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  linkSymbol: Nothing,
  linkSymbolSize: Nothing,
  scaling: Nothing,
  gravity: Nothing,
  draggable: Nothing,
  large: Nothing,
  useWorker: Nothing,
  steps: Nothing,
  ribbonType: Nothing
  }
forceRecEncode :: ForceSeriesRec -> Array JAssoc
forceRecEncode r = [
  "categories" := r.categories,
  "nodes" := r.nodes,
  "links" := r.links,
  "matrix" := r.matrix,
  "center" := r.center,
  "size" := r.size,
  "minRadius" := r.minRadius,
  "maxRadius" := r.maxRadius,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "linkSymbol" := r.linkSymbol,
  "linkSymbolSize" := r.linkSymbolSize,
  "scaling" := r.scaling,
  "gravity" := r.gravity,
  "draggable" := r.draggable,
  "large" := r.large,
  "useWorker" := r.useWorker,
  "steps" := r.steps,
  "ribbonType" := r.ribbonType
  ]

decodeForceSeriesRec :: JObject -> Either String ForceSeriesRec
decodeForceSeriesRec o =
  { categories: _
  , nodes: _
  , links: _
  , matrix: _
  , center: _
  , size: _
  , minRadius: _
  , maxRadius: _
  , symbol: _
  , symbolSize: _
  , linkSymbol: _
  , linkSymbolSize: _
  , scaling: _
  , gravity: _
  , draggable: _
  , large: _
  , useWorker: _
  , steps: _
  , ribbonType: _} <$>
  (o .? "categories") <*>
  (o .? "nodes") <*>
  (o .? "links") <*>
  (o .? "matrix") <*>
  (o .? "center") <*>
  (o .? "size") <*>
  (o .? "minRadius") <*>
  (o .? "maxRadius") <*>
  (o .? "symbol") <*>
  (o .? "symbolSize") <*>
  (o .? "linkSymbol") <*>
  (o .? "linkSymbolSize") <*>
  (o .? "scaling") <*>
  (o .? "gravity") <*>
  (o .? "draggable") <*>
  (o .? "large") <*>
  (o .? "useWorker") <*>
  (o .? "steps") <*>
  (o .? "ribbonType")
                      
type MapSeriesRec = {
  "data" ::Maybe (Array ItemData),
  
  selectedMode :: Maybe SelectedMode,
  mapType :: Maybe String,
  hoverable :: Maybe Boolean,
  dataRangeHoverLink :: Maybe Boolean,
  mapLocation :: Maybe Location,
  mapValueCalculation :: Maybe MapValueCalculation,
  mapValuePrecision :: Maybe Number,
  showLegendSymbol :: Maybe Boolean,
  roam :: Maybe Roam,
  scaleLimit :: Maybe MinMax,
  nameMap :: Maybe (StrMap String),
  textFixed ::  Maybe (StrMap (Tuple Number Number)),
  geoCoord :: Maybe (StrMap (Tuple Number Number))
  }
mapSeriesDefault :: MapSeriesRec
mapSeriesDefault = {
  "data": Nothing,
  selectedMode: Nothing,
  mapType: Nothing,
  hoverable: Nothing,
  dataRangeHoverLink: Nothing,
  mapLocation: Nothing,
  mapValueCalculation: Nothing,
  mapValuePrecision: Nothing,
  showLegendSymbol: Nothing,
  roam: Nothing,
  scaleLimit: Nothing,
  nameMap: Nothing,
  textFixed: Nothing,
  geoCoord: Nothing
  }
mapRecEncode :: MapSeriesRec -> Array JAssoc
mapRecEncode r = [
  "data" := r.data,
  "selectedMode" := r.selectedMode,
  "mapType" := r.mapType,
  "hoverable" := r.hoverable,
  "dataRangeHoverLink" := r.dataRangeHoverLink,
  "mapLocation" := r.mapLocation,
  "mapValueCalculation" := r.mapValueCalculation,
  "mapValuePrecision" := r.mapValuePrecision,
  "showLegendSymbol" := r.showLegendSymbol,
  "roam" := r.roam,
  "scaleLimit" := r.scaleLimit,
  "nameMap" := r.nameMap,
  "textFixed" := r.textFixed,
  "geoCoord" := r.geoCoord
  ]


decodeMapSeriesRec :: JObject -> Either String MapSeriesRec
decodeMapSeriesRec o =
  { "data": _
  , selectedMode: _
  , mapType: _
  , hoverable: _
  , dataRangeHoverLink: _
  , mapLocation: _
  , mapValueCalculation: _
  , mapValuePrecision: _
  , showLegendSymbol: _
  , roam: _
  , scaleLimit: _
  , nameMap: _
  , textFixed: _
  , geoCoord: _ } <$>
  (o .? "data") <*>
  (o .? "selectedMode") <*>
  (o .? "mapType") <*>
  (o .? "hoverable") <*>
  (o .? "dataRangeHoverLink") <*>
  (o .? "mapLocation") <*>
  (o .? "mapValueCalculation") <*>
  (o .? "mapValuePrecision") <*>
  (o .? "showLegendSymbol") <*>
  (o .? "roam") <*>
  (o .? "scaleLimit") <*>
  (o .? "nameMap") <*>
  (o .? "textFixed") <*>
  (o .? "geoCoord")

type GaugeSeriesRec = {
  "data" ::Maybe (Array ItemData),

  center :: Maybe (Tuple Number Number),
  radius :: Maybe Radius,
  startAngle :: Maybe Number,
  endAngle :: Maybe Number,
  min :: Maybe Number,
  max :: Maybe Number,
  precision :: Maybe Number,
  splitNumber :: Maybe Number,
  axisLine :: Maybe AxisLine,
  axisTick :: Maybe AxisTick,
  axisLabel :: Maybe AxisLabel,
  splitLine :: Maybe SplitLine,
  title :: Maybe Title,
  detail :: Maybe GaugeDetail,
  pointer :: Maybe Pointer,
  legendHoverLink :: Maybe Boolean
  }
gaugeSeriesDefault :: GaugeSeriesRec
gaugeSeriesDefault = {
  "data": Nothing,
  center: Nothing,
  radius: Nothing,
  startAngle: Nothing,
  endAngle: Nothing,
  min: Nothing,
  max: Nothing,
  precision: Nothing,
  splitNumber: Nothing,
  axisLine: Nothing,
  axisTick: Nothing,
  splitLine: Nothing,
  title: Nothing,
  detail: Nothing,
  pointer: Nothing,
  legendHoverLink: Nothing,
  axisLabel: Nothing
  }
gaugeRecEncode :: GaugeSeriesRec -> Array JAssoc
gaugeRecEncode r = [
  "data" := r.data,
  "center" := r.center,
  "radius" := r.radius,
  "startAngle" := r.startAngle,
  "endAngle" := r.endAngle,
  "min" := r.min,
  "max" := r.max,
  "precision" := r.precision,
  "splitNumber" := r.splitNumber,
  "axisLine" := r.axisLine,
  "axisTick" := r.axisTick,
  "splitLine" := r.splitLine,
  "title" := r.title,
  "detail" := r.detail,
  "pointer" := r.pointer,
  "legendHoverLink" := r.legendHoverLink,
  "axisLabel" := r.axisLabel
  ]

decodeGaugeSeriesRec :: JObject -> Either String GaugeSeriesRec
decodeGaugeSeriesRec o =
  { "data": _
  , center: _
  , radius: _
  , startAngle: _
  , endAngle: _
  , min: _
  , max: _
  , precision: _
  , splitNumber: _
  , axisLine: _
  , axisTick: _
  , splitLine: _
  , title: _
  , detail: _
  , pointer: _
  , legendHoverLink: _
  , axisLabel: _ } <$>
  (o .? "data") <*>
  (o .? "center") <*>
  (o .? "radius") <*>
  (o .? "startAngle") <*>
  (o .? "endAngle") <*>
  (o .? "min") <*>
  (o .? "max") <*>
  (o .? "precision") <*>
  (o .? "splitNumber") <*>
  (o .? "axisLine") <*>
  (o .? "axisTick") <*>
  (o .? "splitLine") <*>
  (o .? "title") <*>
  (o .? "detail") <*>
  (o .? "pointer") <*>
  (o .? "legendHoverLink") <*>
  (o .? "axisLabel")
                      
type FunnelSeriesRec = {
  "data" ::Maybe (Array ItemData),

  x :: Maybe PercentOrPixel,
  x2 :: Maybe PercentOrPixel,
  y :: Maybe PercentOrPixel,
  y2 :: Maybe PercentOrPixel,
  width :: Maybe PercentOrPixel,
  height :: Maybe PercentOrPixel,
  funnelAlign :: Maybe HorizontalAlign,
  min :: Maybe Number,
  max :: Maybe Number,
  minSize :: Maybe PercentOrPixel,
  maxSize :: Maybe PercentOrPixel,
  gap :: Maybe Number,
  sort :: Maybe Sort,
  legendHoverLink :: Maybe Boolean
  }
funnelSeriesDefault :: FunnelSeriesRec 
funnelSeriesDefault = {
  "data": Nothing,
  x: Nothing,
  x2: Nothing,
  y: Nothing,
  y2: Nothing,
  width: Nothing,
  height: Nothing,
  funnelAlign: Nothing,
  min: Nothing,
  max: Nothing,
  minSize: Nothing,
  maxSize: Nothing,
  gap: Nothing,
  sort: Nothing,
  legendHoverLink: Nothing
  }
funnelRecEncode :: FunnelSeriesRec -> Array JAssoc
funnelRecEncode r = [
  "data" := r.data,
  "x" := r.x,
  "x2" := r.x2,
  "y" := r.y,
  "y2" := r.y2,
  "width" := r.width,
  "height" := r.height,
  "funnelAlign" := r.funnelAlign,
  "min" := r.min,
  "max" := r.max,
  "minSize" := r.minSize,
  "maxSize" := r.maxSize,
  "gap" := r.gap,
  "sort" := r.sort,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeFunnelSeriesRec :: JObject -> Either String FunnelSeriesRec
decodeFunnelSeriesRec o =
  { "data": _
  , x: _
  , x2: _
  , y: _
  , y2: _
  , width: _
  , height: _
  , funnelAlign: _
  , min: _
  , max: _
  , minSize: _
  , maxSize: _
  , gap: _
  , sort: _
  , legendHoverLink: _ } <$>
  (o .? "data") <*>
  (o .? "x") <*>
  (o .? "x2") <*>
  (o .? "y") <*>
  (o .? "y2") <*>
  (o .? "width") <*>
  (o .? "height") <*>
  (o .? "funnelAlign") <*>
  (o .? "min") <*>
  (o .? "max") <*>
  (o .? "minSize") <*>
  (o .? "maxSize") <*>
  (o .? "gap") <*>
  (o .? "sort") <*>
  (o .? "legendHoverLink")
                       
type EventRiverSeriesRec = {
  eventList :: Maybe (Array OneEvent),

  xAxisIndex :: Maybe Number,
  weight :: Maybe Number,
  legendHoverLink :: Maybe Boolean
  }
eventRiverSeriesDefault :: EventRiverSeriesRec
eventRiverSeriesDefault = {
  eventList: Nothing,
  xAxisIndex: Nothing,
  weight: Nothing,
  legendHoverLink: Nothing
  }
eventRiverRecEncode :: EventRiverSeriesRec -> Array JAssoc
eventRiverRecEncode r = [
  "eventList" := r.eventList,
  "xAxisIndex" := r.xAxisIndex,
  "weight" := r.weight,
  "legendHoverLink" := r.legendHoverLink
  ]

decodeEventRiverSeriesRec :: JObject -> Either String EventRiverSeriesRec
decodeEventRiverSeriesRec o =
  { eventList: _
  , xAxisIndex: _
  , weight: _
  , legendHoverLink: _ } <$>
  (o .? "eventList") <*>
  (o .? "xAxisIndex") <*>
  (o .? "weight") <*>
  (o .? "legendHoverLink")

specialForSeries :: Series -> Array JAssoc
specialForSeries series =
  case series of
    LineSeries {lineSeries = s} -> lineRecEncode s
    BarSeries {barSeries = s} -> barRecEncode s
    ScatterSeries {scatterSeries = s} -> scatterRecEncode s
    CandlestickSeries {candlestickSeries = s} -> candlestickRecEncode s
    PieSeries {pieSeries = s} -> pieRecEncode s
    RadarSeries {radarSeries = s} -> radarRecEncode s
    ChordSeries {chordSeries = s} -> chordRecEncode s
    ForceSeries {forceSeries = s} -> forceRecEncode s
    MapSeries {mapSeries = s} -> mapRecEncode s
    GaugeSeries {gaugeSeries = s} -> gaugeRecEncode s
    FunnelSeries {funnelSeries = s} -> funnelRecEncode s
    EventRiverSeries {eventRiverSeries = s} -> eventRiverRecEncode s

instance encodeSeries :: EncodeJson Series where
  encodeJson series =
    fromObject $ fromList $ toList $ concat
    [
      universalForSeries series,
      typeForSeries series,
      specialForSeries series
    ]


instance decodeSeries :: DecodeJson Series where
  decodeJson json = do
    obj <- decodeJson json
    u <- { name: _
         , tooltip: _
         , clickable: _
         , itemStyle: _
         , markPoint: _
         , markLine: _ } <$>
         (obj .? "name") <*>
         (obj .? "tooltip") <*>
         (obj .? "clickable") <*>
         (obj .? "itemStyle") <*>
         (obj .? "markPoint") <*>
         (obj .? "markLine")
    ty <- obj .? "type"
    case ty of
      "line" -> LineSeries <$> ({common: u, lineSeries: _} <$> decodeLineRec obj)
      "bar" -> BarSeries <$> ({common: u, barSeries: _} <$> decodeBarRec obj)
      "scatter" -> ScatterSeries <$> ({common:u, scatterSeries: _} <$> decodeScatterSeriesRec obj)
      "k" -> CandlestickSeries <$> ({common:u, candlestickSeries: _} <$> decodeCandleStickSeries obj)
      "pie" -> PieSeries <$> ({common: u, pieSeries: _} <$> decodePieSeriesRec obj)
      "radar" -> RadarSeries <$> ({common: u, radarSeries: _} <$> decodeRadarSeriesRec obj)
      "chord" -> ChordSeries <$> ({common: u, chordSeries: _} <$> decodeChordSeriesRec obj)
      "force" -> ForceSeries <$> ({common: u, forceSeries: _} <$> decodeForceSeriesRec obj)
      "map" -> MapSeries <$> ({common: u, mapSeries: _} <$> decodeMapSeriesRec obj)
      "gauge" -> GaugeSeries <$> ({common: u, gaugeSeries: _} <$> decodeGaugeSeriesRec obj)
      "funnel" -> FunnelSeries <$> ({common: u, funnelSeries: _} <$> decodeFunnelSeriesRec obj)
      "eventRiver" -> EventRiverSeries <$> ({common: u, eventRiverSeries: _} <$> decodeEventRiverSeriesRec obj)
      

foreign import setSeriesImpl :: forall e. Fn3 (Array Json) Boolean EChart (Eff e EChart)

setSeries :: forall e. Array Series -> Boolean -> EChart -> Eff e EChart
setSeries series merge chart =
  runFn3 setSeriesImpl (unnull <<< encodeJson <$> series) merge chart
