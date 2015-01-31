module ECharts.Options (
  Option(..),
  OptionRec(),
  optionDefault,
  setOption
  ) where

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
import ECharts.Utils
import ECharts.Effects

{- To set second series tooltip
     Option{series = Just [Nothing,
        Just $ SomeSeries
                 universalSeriesDefault{tooltip = Just myTooltip}
                 someSeriesDefault 
       }
   It will erase all series data if we use [] as zero in updating
   i.e. legend.
-}

type OptionRec = {
    backgroundColor :: Maybe Color,
    color :: Maybe [Color],
    renderAsImage :: Maybe Boolean,
    calculable :: Maybe Boolean,
    animation :: Maybe Boolean,

    timeline :: Maybe Timeline,
    tooltip :: Maybe Tooltip,
    toolbox :: Maybe Toolbox,
    title :: Maybe Title,
    legend :: Maybe Legend,
    dataRange :: Maybe DataRange,
    dataZoom :: Maybe DataZoom,
    roamController :: Maybe RoamController,
    grid :: Maybe Grid,
    xAxis :: Maybe Axises,
    yAxis :: Maybe Axises,
    polar :: Maybe [Polar],

    series :: Maybe [Maybe Series]
    }

newtype Option = Option OptionRec
   

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

optionDefault :: OptionRec
optionDefault = {
  backgroundColor: Nothing,
  color: Nothing,
  renderAsImage: Nothing,
  calculable: Nothing,
  animation: Nothing,
  series: Nothing,
  timeline: Nothing,
  tooltip: Nothing,
  toolbox: Nothing,
  title: Nothing,
  legend: Nothing,
  dataRange: Nothing,
  dataZoom: Nothing,
  roamController: Nothing,
  grid: Nothing,
  xAxis: Nothing,
  yAxis: Nothing,
  polar: Nothing
  }

foreign import setOptionImpl """
function setOptionImpl(option, notMerge, chart) {
  return function() {
if (!window.options) window.options = [];
window.options.push(option);
    return chart.setOption(option, notMerge);
  };
}
""" :: forall e.
       Fn3 Json Boolean EChart (Eff (echartSetOption::EChartOptionSet|e) EChart)

setOption :: forall e.
             Option ->
             Boolean ->
             EChart ->
             Eff (echartSetOption::EChartOptionSet|e) EChart
setOption opts notMerge chart = runFn3 setOptionImpl 
                                (unnull <<< encodeJson $ opts) notMerge chart

