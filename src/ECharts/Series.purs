module ECharts.Series where

import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap

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

newtype Series =
  Series {
    -- universal
    "type" :: ChartType,
    "name" :: Maybe String,
    "tooltip" :: Maybe Tooltip,
    "clickable" :: Maybe Boolean,
    "itemStyle" :: Maybe ItemStyle,
    "markPoint" :: Maybe [MarkPoint],
    "markLine" :: Maybe [MarkLine],
    "data" :: Maybe [ItemData],
    -- cartesian
    "stack" :: Maybe String,
    "xAxisIndex" :: Maybe Number,
    "yAxisIndex" :: Maybe Number,
    "barGap" :: Maybe PercentOrPixel,
    "barCategoryGap" :: Maybe PercentOrPixel,
    "barMinWidth" :: Maybe Number,
    "barMinHeight" :: Maybe Number,
    "barWidth" :: Maybe Number,
    "barHeight" :: Maybe Number,
    "symbol" :: Maybe Symbol,
    "symbolRotate" :: Maybe Boolean,
    "symbolSize" :: Maybe SymbolSize,
    "showAllSymbol" :: Maybe Boolean,
    "smooth" :: Maybe Boolean,
    "large" :: Maybe Boolean,
    "largeThreshold" :: Maybe Number,
    "legendHoverLink" :: Maybe Boolean,
    
    -- pie
    "center" :: Maybe (Tuple Number Number),
    "radius" :: Maybe Radius,
    "startAngle" :: Maybe Number,
    "minAngle" :: Maybe Number,
    "clockWise" :: Maybe Boolean,
    "roseType" :: Maybe RoseType,
    "selectedOffset" :: Maybe Number,
    "selectedMode" :: Maybe SelectedMode,
    -- radar
    "polarIndex" :: Maybe Number,
    -- map
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
    "geoCoord" :: Maybe (StrMap (Tuple Number Number)) ,

    -- force
    "categories" :: Maybe [ForceCategory],
    "nodes" :: Maybe [Node],
    "links" :: Maybe [Link],
    "matrix" :: Maybe Matrix,
    "size" :: Maybe Number,
    "minRadius" :: Maybe Number,
    "maxRadius" :: Maybe Number,
    "linkSymbol" :: Maybe Symbol,
    "linkSymbolSize" :: Maybe Symbol,
    "scaling" :: Maybe Number,
    "gravity" :: Maybe Number,
    "draggable" :: Maybe Number,
    "useWorker" :: Maybe Boolean,
    "steps" :: Maybe Number     ,

    -- chord
    "ribbonType" :: Maybe Boolean,
    "showScale" :: Maybe Boolean,
    "showScaleText" :: Maybe Boolean,
    "padding" :: Maybe Number,
    "sort" :: Maybe Sort,
    "sortSub" :: Maybe Sort,

    -- gauge
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

    -- funnel

    "x" :: Maybe PercentOrPixel,
    "x2" :: Maybe PercentOrPixel,
    "y" :: Maybe PercentOrPixel,
    "y2" :: Maybe PercentOrPixel,
    "width" :: Maybe PercentOrPixel,
    "height" :: Maybe PercentOrPixel,
    "funnelAlign" :: Maybe HorizontalAlign,
    "minSize" :: Maybe PercentOrPixel,
    "maxSize" :: Maybe PercentOrPixel,
    "gap" :: Maybe Number,
    "weight" :: Maybe Number,

    -- eventRiver
    "eventList" :: Maybe [OneEvent]
    }

emptySeries :: ChartType -> _
emptySeries chartType = {
  "type": chartType,
  "name": Nothing,
  "tooltip": Nothing,
  "clickable": Nothing,
  "itemStyle": Nothing,
  "markPoint": Nothing,
  "markLine": Nothing,
  "data": Nothing,
  "stack": Nothing,
  "xAxisIndex": Nothing,
  "yAxisIndex": Nothing,
  "barGap": Nothing,
  "barCategoryGap": Nothing,
  "barMinWidth": Nothing,
  "barMinHeight": Nothing,
  "barWidth": Nothing,
  "barHeight": Nothing,
  "symbol": Nothing,
  "symbolSize": Nothing,
  "symbolRotate": Nothing,
  "showAllSymbol": Nothing,
  "smooth": Nothing,
  "large": Nothing,
  "largeThreshold": Nothing,
  "legendHoverLink": Nothing,
  "center": Nothing,
  "radius": Nothing,
  "startAngle": Nothing,
  "minAngle": Nothing,
  "roseType": Nothing,
  "selectedOffset": Nothing,
  "selectedMode": Nothing,
  "polarIndex": Nothing,
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
  "geoCoord": Nothing,
  "categories": Nothing,
  "nodes": Nothing,
  "links": Nothing,
  "matrix": Nothing,
  "size": Nothing,
  "minRadius": Nothing,
  "maxRadius": Nothing,
  "linkSymbol": Nothing,
  "linkSymbolSize": Nothing,
  "scaling": Nothing,
  "gravity": Nothing,
  "draggable": Nothing,
  "useWorker": Nothing,
  "steps": Nothing,
  "ribbonType": Nothing,
  "showScale": Nothing,
  "showScaleText": Nothing,
  "padding": Nothing,
  "sort": Nothing,
  "sortSub": Nothing,
  "clockWise": Nothing,
  "endAngle": Nothing,
  "min": Nothing,
  "max": Nothing,
  "precision": Nothing,
  "splitNumber": Nothing,
  "axisLine": Nothing,
  "axisTick": Nothing,
  "axisLabel": Nothing,
  "splitLine": Nothing,
  "title": Nothing,
  "detail": Nothing,
  "x": Nothing,
  "x2": Nothing,
  "y": Nothing,
  "y2": Nothing,
  "width": Nothing,
  "height": Nothing,
  "funnelAlign": Nothing,
  "minSize": Nothing,
  "maxSize": Nothing,
  "gap": Nothing,
  "weight": Nothing,
  "eventList": Nothing
  }


instance seriesEncodeJson :: EncodeJson Series where
  encodeJson (Series obj) =
    fromObject $ fromList $
    [
      "type" := obj.type,
      "name" := obj.name,
      "tooltip" := obj.tooltip,
      "clickable" := obj.clickable,
      "itemStyle" := obj.itemStyle,
      "markPoint" := obj.markPoint,
      "markLine" := obj.markLine,
      "data" := obj.data,
      "stack" := obj.stack,
      "xAxisIndex" := obj.xAxisIndex,
      "yAxisIndex" := obj.yAxisIndex,
      "barGap" := obj.barGap,
      "barCategoryGap" := obj.barCategoryGap,
      "barMinWidth" := obj.barMinWidth,
      "barMinHeight" := obj.barMinHeight,
      "barWidth" := obj.barWidth,
      "barHeight" := obj.barHeight,
      "symbol" := obj.symbol,
      "symbolSize" := obj.symbolSize,
      "symbolRotate" := obj.symbolRotate,
      "showAllSymbol" := obj.showAllSymbol,
      "smooth" := obj.smooth,
      "large" := obj.large,
      "largeThreshold" := obj.largeThreshold,
      "legendHoverLink" := obj.legendHoverLink,
      "center" := obj.center,
      "radius" := obj.radius,
      "startAngle" := obj.startAngle,
      "minAngle" := obj.minAngle,
      "roseType" := obj.roseType,
      "selectedOffset" := obj.selectedOffset,
      "selectedMode" := obj.selectedMode,
      "polarIndex" := obj.polarIndex,
      "mapType" := obj.mapType,
      "hoverable" := obj.hoverable,
      "dataRangeHoverLink" := obj.dataRangeHoverLink,
      "mapLocation" := obj.mapLocation,
      "mapValueCalculation" := obj.mapValueCalculation,
      "mapValuePrecision" := obj.mapValuePrecision,
      "showLegendSymbol" := obj.showLegendSymbol,
      "roam" := obj.roam,
      "scaleLimit" := obj.scaleLimit,
      "nameMap" := obj.nameMap,
      "textFixed" := obj.textFixed,
      "geoCoord" := obj.geoCoord,
      "categories" := obj.categories,
      "nodes" := obj.nodes,
      "links" := obj.links,
      "matrix" := obj.matrix,
      "size" := obj.size,
      "minRadius" := obj.minRadius,
      "maxRadius" := obj.maxRadius,
      "linkSymbol" := obj.linkSymbol,
      "linkSymbolSize" := obj.linkSymbolSize,
      "scaling" := obj.scaling,
      "gravity" := obj.gravity,
      "draggable" := obj.draggable,
      "useWorker" := obj.useWorker,
      "steps" := obj.steps,
      "ribbonType" := obj.ribbonType,
      "showScale" := obj.showScale,
      "showScaleText" := obj.showScaleText,
      "padding" := obj.padding,
      "sort" := obj.sort,
      "sortSub" := obj.sortSub,
      "clockWise" := obj.clockWise,
      "endAngle" := obj.endAngle,
      "min" := obj.min,
      "max" := obj.max,
      "precision" := obj.precision,
      "splitNumber" := obj.splitNumber,
      "axisLine" := obj.axisLine,
      "axisTick" := obj.axisTick,
      "axisLabel" := obj.axisLabel,
      "splitLine" := obj.splitLine,
      "title" := obj.title,
      "detail" := obj.detail,
      "x" := obj.x,
      "x2" := obj.x2,
      "y" := obj.y,
      "y2" := obj.y2,
      "width" := obj.width,
      "height" := obj.height,
      "funnelAlign" := obj.funnelAlign,
      "minSize" := obj.minSize,
      "maxSize" := obj.maxSize,
      "gap" := obj.gap,
      "weight" := obj.weight,
      "eventList" := obj.eventList
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
  runFn3 setSeriesImpl (encodeJson <$> series) merge chart

foreign import getSeriesImpl """
function getSeriesImpl(chart) {
  return function() {
    return chart.getSeries();
  };
}
""" :: forall e. EChart -> Eff e [Json]

getSeries = getSeriesImpl

