module ECharts.Options
  ( Option(..)
  , OptionRec
  , optionDefault
  , setOption
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.StrMap as SM

import ECharts.Chart (EChart)
import ECharts.Color (Color)
import ECharts.Series (Series)
import ECharts.Timeline (Timeline)
import ECharts.Toolbox (Toolbox)
import ECharts.Tooltip (Tooltip)
import ECharts.Title (Title)
import ECharts.Legend (Legend)
import ECharts.DataRange (DataRange)
import ECharts.DataZoom (DataZoom)
import ECharts.RoamController (RoamController)
import ECharts.Grid (Grid)
import ECharts.Axis (Polar, Axises)
import ECharts.Utils (unnull)
import ECharts.Effects (ECHARTS)

{- To set second series tooltip
     Option{series = Just [Nothing,
        Just $ SomeSeries
                 universalSeriesDefault{tooltip = Just myTooltip}
                 someSeriesDefault
       }
   It will erase all series data if we use [] as zero in updating
   i.e. legend.
-}

type OptionRec =
  { backgroundColor ∷ Maybe Color
  , color ∷ Maybe (Array Color)
  , renderAsImage ∷ Maybe Boolean
  , calculable ∷ Maybe Boolean
  , animation ∷ Maybe Boolean

  , timeline ∷ Maybe Timeline
  , tooltip ∷ Maybe Tooltip
  , toolbox ∷ Maybe Toolbox
  , title ∷ Maybe Title
  , legend ∷ Maybe Legend
  , dataRange ∷ Maybe DataRange
  , dataZoom ∷ Maybe DataZoom
  , roamController ∷ Maybe RoamController
  , grid ∷ Maybe Grid
  , xAxis ∷ Maybe Axises
  , yAxis ∷ Maybe Axises
  , polar ∷ Maybe (Array Polar)

  , series ∷ Maybe (Array (Maybe Series))
  }

newtype Option = Option OptionRec


instance optionsEncodeJson ∷ EncodeJson Option where
  encodeJson (Option opts) =
    encodeJson
      $ SM.fromFoldable
          [ "backgroundColor" := opts.backgroundColor
          , "color" := opts.color
          , "renderAsImage" := opts.renderAsImage
          , "calculable" := opts.calculable
          , "animation" := opts.animation
          , "series" := opts.series
          , "timeline" := opts.timeline
          , "tooltip" := opts.tooltip
          , "toolbox" := opts.toolbox
          , "title" := opts.title
          , "legend" := opts.legend
          , "dataRange" := opts.dataRange
          , "dataZoom" := opts.dataZoom
          , "roamController" := opts.roamController
          , "grid" := opts.grid
          , "xAxis" := opts.xAxis
          , "yAxis" := opts.yAxis
          , "polar" := opts.polar
          ]

instance optionsDecodeJson ∷ DecodeJson Option where
  decodeJson json = do
    obj ← decodeJson json
    r ← { backgroundColor: _
        , color: _
        , renderAsImage: _
        , calculable: _
        , animation: _
        , series: _
        , timeline: _
        , tooltip: _
        , toolbox: _
        , title: _
        , legend: _
        , dataRange: _
        , dataZoom: _
        , roamController: _
        , grid: _
        , xAxis: _
        , yAxis: _
        , polar: _ }
        <$> (obj .? "backgroundColor")
        <*> (obj .? "color")
        <*> (obj .? "renderAsImage")
        <*> (obj .? "calculable")
        <*> (obj .? "animation")
        <*> (obj .? "series")
        <*> (obj .? "timeline")
        <*> (obj .? "tooltip")
        <*> (obj .? "toolbox")
        <*> (obj .? "title")
        <*> (obj .? "legend")
        <*> (obj .? "dataRange")
        <*> (obj .? "dataZoom")
        <*> (obj .? "roamController")
        <*> (obj .? "grid")
        <*> (obj .? "xAxis")
        <*> (obj .? "yAxis")
        <*> (obj .? "polar")
    pure $ Option r


optionDefault ∷ OptionRec
optionDefault =
  { backgroundColor: Nothing
  , color: Nothing
  , renderAsImage: Nothing
  , calculable: Nothing
  , animation: Nothing
  , series: Nothing
  , timeline: Nothing
  , tooltip: Nothing
  , toolbox: Nothing
  , title: Nothing
  , legend: Nothing
  , dataRange: Nothing
  , dataZoom: Nothing
  , roamController: Nothing
  , grid: Nothing
  , xAxis: Nothing
  , yAxis: Nothing
  , polar: Nothing
  }

foreign import setOptionImpl
  ∷ ∀ e. Fn3 Json Boolean EChart (Eff (echarts ∷ ECHARTS|e) EChart)

setOption
  ∷ ∀ e
  . Option
  → Boolean
  → EChart
  → Eff (echarts ∷ ECHARTS|e) EChart
setOption opts notMerge chart =
  runFn3 setOptionImpl (unnull <<< encodeJson $ opts) notMerge chart
