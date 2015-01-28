module ECharts.Series (
  setSeries,
  Series(..),
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


import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap
import Data.Array (concat)

import ECharts.Common
import ECharts.Coords
import ECharts.Chart
import ECharts.Tooltip
import ECharts.Type
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

data Series = LineSeries
              {common :: UniversalSeriesRec, special :: LineSeriesRec}
            | BarSeries
              {common :: UniversalSeriesRec, special ::  BarSeriesRec}
            | ScatterSeries
              {common :: UniversalSeriesRec, special :: ScatterSeriesRec}
            | CandlestickSeries 
              {common :: UniversalSeriesRec, special :: CandlestickSeriesRec}
            | PieSeries
              {common :: UniversalSeriesRec, special ::  PieSeriesRec}
            | RadarSeries
              {common :: UniversalSeriesRec, special :: RadarSeriesRec}
            | ChordSeries
              {common :: UniversalSeriesRec, special :: ChordSeriesRec}
            | ForceSeries
              {common :: UniversalSeriesRec, special :: ForceSeriesRec}
            | MapSeries
              {common :: UniversalSeriesRec, special :: MapSeriesRec}
            | GaugeSeries
              {common :: UniversalSeriesRec, special :: GaugeSeriesRec}
            | FunnelSeries
              {common :: UniversalSeriesRec, special :: FunnelSeriesRec}
            | EventRiverSeries
              {common :: UniversalSeriesRec, special :: EventRiverSeriesRec}

typeForSeries :: Series -> [JAssoc]
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
  "name" :: Maybe String,
  "tooltip" :: Maybe Tooltip,
  "clickable" :: Maybe Boolean,
  "itemStyle" :: Maybe ItemStyle,
  "markPoint" :: Maybe MarkPoint,
  "markLine" :: Maybe MarkLine
  }

universalSeriesDefault = {
  "name": Nothing,
  "tooltip": Nothing,
  "clickable": Nothing,
  "itemStyle": Nothing,
  "markPoint": Nothing,
  "markLine": Nothing
  }
universalRecEncode :: UniversalSeriesRec -> [JAssoc]
universalRecEncode r =
   [
    "name" := r.name,
    "tooltip" := r.tooltip,
    "clickable" := r.clickable,
    "itemStyle" := r.itemStyle,
    "markPoint" := r.markPoint,
    "markLine" := r.markLine
    ]
universalForSeries :: Series -> [JAssoc]
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
  "data" :: Maybe [ItemData],
  "stack" :: Maybe String,
  "xAxisIndex" :: Maybe Number,
  "yAxisIndex" :: Maybe Number,
  "symbol" :: Maybe Symbol,
  "symbolSize" :: Maybe SymbolSize,
  "symbolRotate" :: Maybe Boolean,
  "showAllSymbol" :: Maybe Boolean,
  "smooth" :: Maybe Boolean,
  "legendHoverLink" :: Maybe Boolean
  }

lineSeriesDefault  = {
  "data": Nothing,
  "stack": Nothing,
  "xAxisIndex": Nothing,
  "yAxisIndex": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "symbolRotate": Nothing,
  "showAllSymbol": Nothing,
  "smooth": Nothing,
  "legendHoverLink": Nothing
  }
lineRecEncode :: LineSeriesRec -> [JAssoc]
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


type BarSeriesRec = {
  "data" :: Maybe [ItemData],
  "stack" :: Maybe String,
  "xAxisIndex" :: Maybe Number,
  "yAxisIndex" :: Maybe Number,
  "barGap" :: Maybe PercentOrPixel,
  "barCategoryGap" :: Maybe PercentOrPixel,
  "barMinHeight" :: Maybe Number,
  "barWidth" :: Maybe Number,
  "barMaxWidth" :: Maybe Number,
  "legendHoverLink" :: Maybe Boolean
  }

barSeriesDefault = {
  "data": Nothing,
  "stack": Nothing,
  "xAxisIndex": Nothing,
  "yAxisIndex": Nothing,
  "barGap": Nothing,
  "barCategoryGap": Nothing,
  "barMinHeight": Nothing,
  "barWidth": Nothing,
  "barMaxWidth": Nothing,
  "legendHoverLink": Nothing
  }
barRecEncode :: BarSeriesRec -> [JAssoc]
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


type ScatterSeriesRec = {
  "data" :: Maybe [ItemData],
  "xAxisIndex" :: Maybe Number,
  "yAxisIndex" :: Maybe Number,
  "symbol" :: Maybe Symbol,
  "symbolSize" :: Maybe SymbolSize,
  "symbolRotate" :: Maybe Boolean,
  "large" :: Maybe Boolean,
  "largeThreshold" :: Maybe Number,
  "legendHoverLink" :: Maybe Boolean
  }

scatterSeriesDefault = {
  "data": Nothing,
  "xAxisIndex": Nothing,
  "yAxisIndex": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "symbolRotate": Nothing,
  "large": Nothing,
  "largeThreshold": Nothing,
  "legendHoverLink": Nothing
  }

scatterRecEncode :: ScatterSeriesRec -> [JAssoc]
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

type CandlestickSeriesRec = {
  "data" :: Maybe [ItemData],
  "xAxisIndex" :: Maybe Number,
  "yAxisIndex" :: Maybe Number,
  "barMinHeight" :: Maybe Number,
  "barWidth" :: Maybe Number,
  "barMaxWidth" :: Maybe Number
  }

candlestickSeriesDefault = {
  "data": Nothing,
  "xAxisIndex": Nothing,
  "yAxisIndex": Nothing,
  "barMinHeight": Nothing,
  "barWidth": Nothing,
  "barMaxWidth": Nothing
  }
candlestickRecEncode :: CandlestickSeriesRec -> [JAssoc]
candlestickRecEncode r = [
  "data" := r.data,
  "xAxisIndex" := r.xAxisIndex,
  "yAxisIndex" := r.yAxisIndex,
  "barMinHeight" := r.barMinHeight,
  "barWidth" := r.barWidth,
  "barMaxWidth" := r.barMaxWidth
  ]
                                 
type PieSeriesRec = {
  "data" :: Maybe [ItemData],
  "center" :: Maybe Center,
  "radius" :: Maybe Radius,
  "startAngle" :: Maybe Number,
  "minAngle" :: Maybe Number,
  "clockWise" :: Maybe Boolean,
  "roseType" :: Maybe RoseType,
  "selectedOffset" :: Maybe Number,
  "selectedMode" :: Maybe SelectedMode,
  "legendHoverLink" :: Maybe Boolean
  }

pieSeriesDefault = {
  "data": Nothing,
  "center": Nothing,
  "radius": Nothing,
  "startAngle": Nothing,
  "minAngle": Nothing,
  "clockWise": Nothing,
  "roseType": Nothing,
  "selectedOffset": Nothing,
  "selectedMode": Nothing,
  "legendHoverLink": Nothing
  }

pieRecEncode :: PieSeriesRec -> [JAssoc]
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

                 
type RadarSeriesRec = {
  "data" :: Maybe [ItemData],

  "polarIndex" :: Maybe Number,
  "symbol" :: Maybe Symbol,
  "symbolSize" :: Maybe SymbolSize,
  "symbolRotate" :: Maybe Boolean,
  "legendHoverLink" :: Maybe Boolean
  }


radarSeriesDefault = {
  "data": Nothing,
  "polarIndex": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "symbolRotate": Nothing,
  "legendHoverLink": Nothing
  }
radarRecEncode :: RadarSeriesRec -> [JAssoc]
radarRecEncode r = [
  "data" := r.data,
  "polarIndex" := r.polarIndex,
  "symbol" := r.symbol,
  "symbolSize" := r.symbolSize,
  "symbolRotate" := r.symbolRotate,
  "legendHoverLink" := r.legendHoverLink
  ]

type ChordSeriesRec = {
  "nodes" :: Maybe [Node],
  "categories" :: Maybe [ForceCategory],
  "links" :: Maybe [Link],
  "matrix" :: Maybe Matrix,

  "data" :: Maybe [ItemData],
  "ribbonType" :: Maybe Boolean,
  "symbol" :: Maybe Symbol,
  "symbolSize" :: Maybe SymbolSize,
  "minRadius" :: Maybe Number,
  "maxRadius" :: Maybe Number,
  "showScale" :: Maybe Boolean,
  "showScaleText" :: Maybe Boolean,
  "padding" :: Maybe Number,
  "sort" :: Maybe Sort,
  "sortSub" :: Maybe Sort,
  "clockWise" :: Maybe Boolean
  }
chordSeriesDefault = {
  "nodes": Nothing,
  "categories": Nothing,
  "links": Nothing,
  "matrix": Nothing,
  "data": Nothing,
  "ribbonType": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "minRadius": Nothing,
  "maxRadius": Nothing,
  "showScale": Nothing,
  "showScaleText": Nothing,
  "padding": Nothing,
  "sort": Nothing,
  "sortSub": Nothing,
  "clockWise": Nothing
  }
chordRecEncode :: ChordSeriesRec -> [JAssoc]
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

type ForceSeriesRec = {
  "categories" :: Maybe [ForceCategory],
  "nodes" :: Maybe [Node],
  "links" :: Maybe [Link],
  "matrix" :: Maybe Matrix,

  "center" :: Maybe Center,
  "size" :: Maybe Number,
  "minRadius" :: Maybe Number,
  "maxRadius" :: Maybe Number,
  "symbol" :: Maybe Symbol,
  "symbolSize" :: Maybe SymbolSize,
  "linkSymbol" :: Maybe Symbol,
  "linkSymbolSize" :: Maybe Symbol,
  "scaling" :: Maybe Number,
  "gravity" :: Maybe Number,
  "draggable" :: Maybe Number,
  "large" :: Maybe Boolean,
  "useWorker" :: Maybe Boolean,
  "steps" :: Maybe Number,
  "ribbonType" :: Maybe Boolean
  }

forceSeriesDefault = {
  "categories": Nothing,
  "nodes": Nothing,
  "links": Nothing,
  "matrix": Nothing,
  "center": Nothing,
  "size": Nothing,
  "minRadius": Nothing,
  "maxRadius": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "linkSymbol": Nothing,
  "linkSymbolSize": Nothing,
  "scaling": Nothing,
  "gravity": Nothing,
  "draggable": Nothing,
  "large": Nothing,
  "useWorker": Nothing,
  "steps": Nothing,
  "ribbonType": Nothing
  }
forceRecEncode :: ForceSeriesRec -> [JAssoc]
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
                      
type MapSeriesRec = {
  "data" :: Maybe [ItemData],
  
  "selectedMode" :: Maybe SelectedMode,
  "mapType" :: Maybe String,
  "hoverable" :: Maybe Boolean,
  "dataRangeHoverLink" :: Maybe Boolean,
  "mapLocation" :: Maybe Location,
  "mapValueCalculation" :: Maybe MapValueCalculation,
  "mapValuePrecision" :: Maybe Number,
  "showLegendSymbol" :: Maybe Boolean,
  "roam" :: Maybe Roam,
  "scaleLimit" :: Maybe MinMax,
  "nameMap" :: Maybe (StrMap String),
  "textFixed" ::  Maybe (StrMap (Tuple Number Number)),
  "geoCoord" :: Maybe (StrMap (Tuple Number Number))
  }

mapSeriesDefault = {
  "data": Nothing,
  "selectedMode": Nothing,
  "mapType": Nothing,
  "hoverable": Nothing,
  "dataRangeHoverLink": Nothing,
  "mapLocation": Nothing,
  "mapValueCalculation": Nothing,
  "mapValuePrecision": Nothing,
  "showLegendSymbol": Nothing,
  "roam": Nothing,
  "scaleLimit": Nothing,
  "nameMap": Nothing,
  "textFixed": Nothing,
  "geoCoord": Nothing
  }
mapRecEncode :: MapSeriesRec -> [JAssoc]
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

type GaugeSeriesRec = {
  "data" :: Maybe [ItemData],

  "center" :: Maybe (Tuple Number Number),
  "radius" :: Maybe Radius,
  "startAngle" :: Maybe Number,
  "endAngle" :: Maybe Number,
  "min" :: Maybe Number,
  "max" :: Maybe Number,
  "precision" :: Maybe Number,
  "splitNumber" :: Maybe Number,
  "axisLine" :: Maybe AxisLine,
  "axisTick" :: Maybe AxisTick,
  "axisLabel" :: Maybe AxisLabel,
  "splitLine" :: Maybe SplitLine,
  "title" :: Maybe Title,
  "detail" :: Maybe GaugeDetail,
  "pointer" :: Maybe Pointer,
  "legendHoverLink" :: Maybe Boolean
  }

gaugeSeriesDefault = {
  "data": Nothing,
  "center": Nothing,
  "radius": Nothing,
  "startAngle": Nothing,
  "endAngle": Nothing,
  "min": Nothing,
  "max": Nothing,
  "precision": Nothing,
  "splitNumber": Nothing,
  "axisLine": Nothing,
  "axisTick": Nothing,
  "splitLine": Nothing,
  "title": Nothing,
  "detail": Nothing,
  "pointer": Nothing,
  "legendHoverLink": Nothing,
  "axisLabel": Nothing
  }
gaugeRecEncode :: GaugeSeriesRec -> [JAssoc]
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
                      
type FunnelSeriesRec = {
  "data" :: Maybe [ItemData],

  "x" :: Maybe PercentOrPixel,
  "x2" :: Maybe PercentOrPixel,
  "y" :: Maybe PercentOrPixel,
  "y2" :: Maybe PercentOrPixel,
  "width" :: Maybe PercentOrPixel,
  "height" :: Maybe PercentOrPixel,
  "funnelAlign" :: Maybe HorizontalAlign,
  "min" :: Maybe Number,
  "max" :: Maybe Number,
  "minSize" :: Maybe PercentOrPixel,
  "maxSize" :: Maybe PercentOrPixel,
  "gap" :: Maybe Number,
  "sort" :: Maybe Sort,
  "legendHoverLink" :: Maybe Boolean
  }

funnelSeriesDefault = {
  "data": Nothing,
  "x": Nothing,
  "x2": Nothing,
  "y": Nothing,
  "y2": Nothing,
  "width": Nothing,
  "height": Nothing,
  "funnelAlign": Nothing,
  "min": Nothing,
  "max": Nothing,
  "minSize": Nothing,
  "maxSize": Nothing,
  "gap": Nothing,
  "sort": Nothing,
  "legendHoverLink": Nothing
  }
funnelRecEncode :: FunnelSeriesRec -> [JAssoc]
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
                       
type EventRiverSeriesRec = {
  "eventList" :: Maybe [OneEvent],

  "xAxisIndex" :: Maybe Number,
  "weight" :: Maybe Number,
  "legendHoverLink" :: Maybe Boolean
  }
  
eventRiverSeriesDefault = {
  "eventList": Nothing,
  "xAxisIndex": Nothing,
  "weight": Nothing,
  "legendHoverLink": Nothing
  }
eventRiverRecEncode :: EventRiverSeriesRec -> [JAssoc]
eventRiverRecEncode r = [
  "eventList" := r.eventList,
  "xAxisIndex" := r.xAxisIndex,
  "weight" := r.weight,
  "legendHoverLink" := r.legendHoverLink
  ]

specialForSeries :: Series -> [JAssoc]
specialForSeries series =
  case series of
    LineSeries {special = s} -> lineRecEncode s
    BarSeries {special = s} -> barRecEncode s
    ScatterSeries {special = s} -> scatterRecEncode s
    CandlestickSeries {special = s} -> candlestickRecEncode s
    PieSeries {special = s} -> pieRecEncode s
    RadarSeries {special = s} -> radarRecEncode s
    ChordSeries {special = s} -> chordRecEncode s
    ForceSeries {special = s} -> forceRecEncode s
    MapSeries {special = s} -> mapRecEncode s
    GaugeSeries {special = s} -> gaugeRecEncode s
    FunnelSeries {special = s} -> funnelRecEncode s
    EventRiverSeries {special = s} -> eventRiverRecEncode s

instance encodeSeries :: EncodeJson Series where
  encodeJson series =
    fromObject $ fromList $ concat
    [
      universalForSeries series,
      typeForSeries series,
      specialForSeries series
    ]
  
foreign import setSeriesImpl """
function setSeriesImpl(series, notMerge, chart) {
  return function() {
    return chart.setSeries(series, notMerge);
  };
}
""" :: forall e. Fn3 [Json] Boolean EChart (Eff e EChart)

setSeries :: forall e. [Series] -> Boolean -> EChart -> Eff e EChart
setSeries series merge chart =
  runFn3 setSeriesImpl (unnull <<< encodeJson <$> series) merge chart
