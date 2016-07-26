module ECharts.Series where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), fst, snd)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField)

import ECharts.Series.Line (LineM, buildLine)
import ECharts.Series.Pie (PieM, buildPie)
import ECharts.Series.Bar (BarM, buildBar)

data SeriesC
  = Pie T.PieSeries
  | Line T.LineSeries
  | Bar T.BarSeries
  | Scatter T.ScatterSeries
  | EffectScatter T.EffectScatterSeries
  | Radar T.RadarSeries
  | TreeMap T.TreeMapSeries
  | BoxPlot T.BoxPlotSeries
  | Candlestick T.CandlestickSeries
  | HeatMap T.HeatMapSeries
  | Map T.MapSeries
  | Parallel T.ParallelSeries
  | Lines T.LinesSeries
  | Graph T.GraphSeries
  | Sankey T.SankeySeries
  | Funnel T.FunnelSeries
  | Gauge T.GaugeSeries

newtype SeriesM a = SeriesM (Writer (Array SeriesC) a)

instance functorSeriesM ∷ Functor SeriesM where
  map f (SeriesM o) = SeriesM $ map f o

instance applySeriesM ∷ Apply SeriesM where
  apply (SeriesM f) (SeriesM o) = SeriesM $ apply f o

instance applicativeSeriesM ∷ Applicative SeriesM where
  pure = SeriesM <<< pure

instance bindSeriesM ∷ Bind SeriesM where
  bind (SeriesM o) f = SeriesM $ o >>= (\(SeriesM o') → o') <<< f

instance monadSeriesM ∷ Monad SeriesM

pieF ∷ T.PieSeries → SeriesM Unit
pieF = SeriesM <<< tell <<< Arr.singleton <<< Pie

pie ∷ PieM Unit → SeriesM Unit
pie = pieF <<< buildPie

lineF ∷ T.LineSeries → SeriesM Unit
lineF = SeriesM <<< tell <<< Arr.singleton <<< Line

line ∷ LineM Unit → SeriesM Unit
line = lineF <<< buildLine

barF ∷ T.BarSeries → SeriesM Unit
barF = SeriesM <<< tell <<< Arr.singleton <<< Bar

bar ∷ BarM Unit → SeriesM Unit
bar = barF <<< buildBar

scatter ∷ T.ScatterSeries → SeriesM Unit
scatter = SeriesM <<< tell <<< Arr.singleton <<< Scatter

effectScatter ∷ T.EffectScatterSeries → SeriesM Unit
effectScatter = SeriesM <<< tell <<< Arr.singleton <<< EffectScatter

radar ∷ T.RadarSeries → SeriesM Unit
radar = SeriesM <<< tell <<< Arr.singleton <<< Radar

treeMap ∷ T.TreeMapSeries → SeriesM Unit
treeMap = SeriesM <<< tell <<< Arr.singleton <<< TreeMap

boxPlot ∷ T.BoxPlotSeries → SeriesM Unit
boxPlot = SeriesM <<< tell <<< Arr.singleton <<< BoxPlot

candlestick ∷ T.CandlestickSeries → SeriesM Unit
candlestick = SeriesM <<< tell <<< Arr.singleton <<< Candlestick

heatMap ∷ T.HeatMapSeries → SeriesM Unit
heatMap = SeriesM <<< tell <<< Arr.singleton <<< HeatMap

-- `map` is already used by `Prelude` :)
mapSeries ∷ T.MapSeries → SeriesM Unit
mapSeries = SeriesM <<< tell <<< Arr.singleton <<< Map

parallel ∷ T.ParallelSeries → SeriesM Unit
parallel = SeriesM <<< tell <<< Arr.singleton <<< Parallel

lines ∷ T.LinesSeries → SeriesM Unit
lines = SeriesM <<< tell <<< Arr.singleton <<< Lines

graph ∷ T.GraphSeries → SeriesM Unit
graph = SeriesM <<< tell <<< Arr.singleton <<< Graph

sankey ∷ T.SankeySeries → SeriesM Unit
sankey = SeriesM <<< tell <<< Arr.singleton <<< Sankey

funnel ∷ T.FunnelSeries → SeriesM Unit
funnel = SeriesM <<< tell <<< Arr.singleton <<< Funnel

gauge ∷ T.GaugeSeries → SeriesM Unit
gauge = SeriesM <<< tell <<< Arr.singleton <<< Gauge

typeAndSerie ∷ SeriesC → Tuple String Foreign
typeAndSerie = case _ of
  Pie f → Tuple "pie" $ T.unPieSeries f
  Bar f → Tuple "bar" $ T.unBarSeries f
  Line f → Tuple "line" $ T.unLineSeries f
  Scatter f → Tuple "scatter" $ T.unScatterSeries f
  EffectScatter f → Tuple "effectScatter" $ T.unEffectScatterSeries f
  Radar f → Tuple "radar" $ T.unRadarSeries f
  TreeMap f → Tuple "treemap" $ T.unTreeMapSeries f
  BoxPlot f → Tuple "boxplot" $ T.unBoxPlotSeries f
  Candlestick f → Tuple "candlestick" $ T.unCandlestickSeries f
  HeatMap f → Tuple "heatmap" $ T.unHeatMapSeries f
  Map f → Tuple "map" $ T.unMapSeries f
  Parallel f → Tuple "parallel" $ T.unParallelSeries f
  Lines f → Tuple "lines" $ T.unLinesSeries f
  Graph f → Tuple "graph" $ T.unGraphSeries f
  Sankey f → Tuple "sankey" $ T.unSankeySeries f
  Funnel f → Tuple "funnel" $ T.unFunnelSeries f
  Gauge f → Tuple "gauge" $ T.unGaugeSeries f


buildSeries ∷ SeriesM Unit → T.Series
buildSeries (SeriesM cs) =
  let
    commands ∷ Array SeriesC
    commands = execWriter cs

    mapFn ∷ SeriesC → Foreign
    mapFn sc =
      let tas = typeAndSerie sc
      in unsafeSetField (snd tas) "type" (toForeign $ fst tas)
  in
    T.Series $ toForeign $ map mapFn commands
