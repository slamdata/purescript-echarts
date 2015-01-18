module ECharts.Options where

import Control.Monad.Eff
import Data.Maybe
import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.StrMap (fromList)

import ECharts.Chart
import ECharts.Color
import ECharts.Series
import ECharts.Timeline
import ECharts.Toolbox
import ECharts.Tooltip
import ECharts.Title
import ECharts.Legend
import ECharts.DataRange
import ECharts.DataZoom
import ECharts.RoamController
import ECharts.Grid
import ECharts.Axis


foreign import data EChartOptionSet :: !

newtype Option =
  Option {
    "backgroundColor" :: Maybe Color,
    "color" :: Maybe [Color],
    "renderAsImage" :: Maybe Boolean,
    "calculable" :: Maybe Boolean,
    "animation" :: Maybe Boolean,

    "timeline" :: Maybe Timeline,
    "tooltip" :: Maybe Tooltip,
    "toolbox" :: Maybe Toolbox,
    "title" :: Maybe Title,
    "legend" :: Maybe Legend,
    "dataRange" :: Maybe DataRange,
    "dataZoom" :: Maybe DataZoom,
    "roamController" :: Maybe RoamController,
    "grid" :: Maybe Grid,
    "xAxis" :: Maybe Axises,
    "yAxis" :: Maybe Axises,
    "polar" :: Maybe [Polar],
    
    "series" :: Maybe [Series]
    }

instance optionsEncodeJson :: EncodeJson Option where
  encodeJson (Option opts) =
    fromObject $ fromList $
    [
      "backgroundColor" := opts.backgroundColor,
      "color" := opts.color,
      "renderAsImage" := opts.renderAsImage,
      "calculable" := opts.calculable,
      "animation" := opts.animation,
      
      "series" := opts.series,

      "timeline" := opts.timeline,
      "tooltip" := opts.tooltip,
      "toolbox" := opts.toolbox,
      "title" := opts.title,
      "legend" := opts.legend,
      "dataRange" := opts.dataRange,
      "dataZoom" := opts.dataZoom,
      "roamController" := opts.roamController,
      "grid" := opts.grid,
      "xAxis" := opts.xAxis,
      "yAxis" := opts.yAxis,
      "polar" := opts.polar
    ]

emptyOptions = {
  "backgroundColor": Nothing,
  "color": Nothing,
  "renderAsImage": Nothing,
  "calculable": Nothing,
  "animation": Nothing,
  "series": Nothing,
  "timeline": Nothing,
  "tooltip": Nothing,
  "toolbox": Nothing,
  "title": Nothing,
  "legend": Nothing,
  "dataRange": Nothing,
  "dataZoom": Nothing,
  "roamController": Nothing,
  "grid": Nothing,
  "xAxis": Nothing,
  "yAxis": Nothing,
  "polar": Nothing
  }

foreign import setOptionImpl """
function setOptionImpl(option, notMerge, chart) {
  return function() {
    var unnull = function(obj) {
      if (obj == null || typeof(obj) != 'object') {
        return obj;
      }
      var temp = new obj.constructor();
      for (var i in obj) {
        if (obj.hasOwnProperty(i) && obj[i] !== null && obj[i] !== undefined) {
          temp[i] = unnull(obj[i]);
        }
      }
      return temp;
    };
    return chart.setOption(unnull(option), notMerge);
  };
}
""" :: forall e.
       Fn3 Json Boolean EChart (Eff (echartSetOption::EChartOptionSet|e) EChart)

setOption :: forall e.
             Option ->
             Boolean ->
             EChart ->
             Eff (echartSetOption::EChartOptionSet|e) EChart
setOption opts notMerge chart = runFn3 setOptionImpl (encodeJson opts) notMerge chart

foreign import getOption """
function getOption(chart) {
  return function() {
    return chart.getOption();
  };
}
""" :: forall e. EChart -> Eff e Json

