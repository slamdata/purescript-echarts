module ECharts.Types where

import Prelude

import Data.Foreign (Foreign, toForeign)

newtype Option = Option Foreign
unOption ∷ Option → Foreign
unOption (Option f) = f

newtype Grid = Grid Foreign
unGrid ∷ Grid → Foreign
unGrid (Grid f) = f

newtype Tooltip = Tooltip Foreign
unTooltip ∷ Tooltip → Foreign
unTooltip (Tooltip f) = f

newtype Legend = Legend Foreign
unLegend ∷ Legend → Foreign
unLegend (Legend f) = f

newtype XAxis = XAxis Foreign
unXAxis ∷ XAxis → Foreign
unXAxis (XAxis f) = f

newtype YAxis = YAxis Foreign
unYAxis ∷ YAxis → Foreign
unYAxis (YAxis f) = f

newtype Series = Series Foreign
unSeries ∷ Series → Foreign
unSeries (Series f) = f

data TooltipTrigger
  = ItemTrigger
  | AxisTrigger

printTooltipTrigger ∷ TooltipTrigger → String
printTooltipTrigger = case _ of
  ItemTrigger → "item"
  AxisTrigger → "axis"

data PixelOrPercent
  = Pixel Int
  | Percent Number

pixelOrPercentToForeign ∷ PixelOrPercent → Foreign
pixelOrPercentToForeign = case _ of
  Pixel i → toForeign i
  Percent n → toForeign $ show n <> "%"

newtype TextStyle = TextStyle Foreign
unTextStyle ∷ TextStyle → Foreign
unTextStyle (TextStyle f) = f

data Orient
  = Vertical
  | Horizontal

printOrient ∷ Orient → String
printOrient = case _ of
  Vertical → "vertical"
  Horizontal → "horizontal"

newtype Item = Item Foreign
unDatum ∷ Item → Foreign
unDatum (Item f) = f

data AxisType
  = Category
  | Value
  | Time
  | Log

printAxisType ∷ AxisType → String
printAxisType = case _ of
  Category → "category"
  Value → "value"
  Time → "time"
  Log → "log"

newtype AxisTick = AxisTick Foreign
unAxisTick ∷ AxisTick → Foreign
unAxisTick (AxisTick f) = f

newtype AxisLabel = AxisLabel Foreign
unAxisLabel ∷ AxisLabel → Foreign
unAxisLabel (AxisLabel f) = f

newtype PieSeries = PieSeries Foreign
unPieSeries ∷ PieSeries → Foreign
unPieSeries (PieSeries f) = f

newtype LineSeries = LineSeries Foreign
unLineSeries ∷ LineSeries → Foreign
unLineSeries (LineSeries f) = f

newtype BarSeries = BarSeries Foreign
unBarSeries ∷ BarSeries → Foreign
unBarSeries (BarSeries f) = f

newtype ScatterSeries = ScatterSeries Foreign
unScatterSeries ∷ ScatterSeries → Foreign
unScatterSeries (ScatterSeries f) = f

newtype EffectScatterSeries = EffectScatterSeries Foreign
unEffectScatterSeries ∷ EffectScatterSeries → Foreign
unEffectScatterSeries (EffectScatterSeries f) = f

newtype RadarSeries = RadarSeries Foreign
unRadarSeries ∷ RadarSeries → Foreign
unRadarSeries (RadarSeries f) = f

newtype TreeMapSeries = TreeMapSeries Foreign
unTreeMapSeries ∷ TreeMapSeries → Foreign
unTreeMapSeries (TreeMapSeries f) = f

newtype BoxPlotSeries = BoxPlotSeries Foreign
unBoxPlotSeries ∷ BoxPlotSeries → Foreign
unBoxPlotSeries (BoxPlotSeries f) = f

newtype CandlestickSeries = CandlestickSeries Foreign
unCandlestickSeries ∷ CandlestickSeries → Foreign
unCandlestickSeries (CandlestickSeries f) = f

newtype HeatMapSeries = HeatMapSeries Foreign
unHeatMapSeries ∷ HeatMapSeries → Foreign
unHeatMapSeries (HeatMapSeries f) = f

newtype MapSeries = MapSeries Foreign
unMapSeries ∷ MapSeries → Foreign
unMapSeries (MapSeries f) = f

newtype ParallelSeries = ParallelSeries Foreign
unParallelSeries ∷ ParallelSeries → Foreign
unParallelSeries (ParallelSeries f) = f

newtype LinesSeries = LinesSeries Foreign
unLinesSeries ∷ LinesSeries → Foreign
unLinesSeries (LinesSeries f) = f

newtype GraphSeries = GraphSeries Foreign
unGraphSeries ∷ GraphSeries → Foreign
unGraphSeries (GraphSeries f) = f

newtype SankeySeries = SankeySeries Foreign
unSankeySeries ∷ SankeySeries → Foreign
unSankeySeries (SankeySeries f) = f

newtype FunnelSeries = FunnelSeries Foreign
unFunnelSeries ∷ FunnelSeries → Foreign
unFunnelSeries (FunnelSeries f) = f

newtype GaugeSeries = GaugeSeries Foreign
unGaugeSeries ∷ GaugeSeries → Foreign
unGaugeSeries (GaugeSeries f) = f

newtype SymbolSize = SymbolSize Foreign
unSymbolSize ∷ SymbolSize → Foreign
unSymbolSize (SymbolSize f) = f

data Symbol
  = Circle
  | Rect
  | RoundRect
  | Triangle
  | Diamond
  | Pin
  | Arrow
  | EmptyCircle

printSymbol ∷ Symbol → String
printSymbol = case _ of
  Circle → "circle"
  Rect → "rect"
  RoundRect → "roundRect"
  Triangle → "triangle"
  Diamond → "diamond"
  Pin → "pin"
  Arrow → "arrow"
  EmptyCircle → "emptyCircle"

newtype ItemStyle = ItemStyle Foreign
unItemStyle ∷ ItemStyle → Foreign
unItemStyle (ItemStyle f) = f

newtype LineStyle = LineStyle Foreign
unLineStyle ∷ LineStyle → Foreign
unLineStyle (LineStyle f) = f

newtype AreaStyle = AreaStyle Foreign
unAreaStyle ∷ AreaStyle → Foreign
unAreaStyle (AreaStyle f) = f

newtype Point = Point { x ∷ PixelOrPercent, y ∷ PixelOrPercent }
pointToForeign ∷ Point → Foreign
pointToForeign (Point {x, y}) =
  toForeign [ pixelOrPercentToForeign x, pixelOrPercentToForeign y ]

newtype Radius = Radius { start ∷ PixelOrPercent, end ∷ PixelOrPercent }
radiusToForeign ∷ Radius → Foreign
radiusToForeign (Radius {start, end}) =
  toForeign [ pixelOrPercentToForeign start, pixelOrPercentToForeign end ]
