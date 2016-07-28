module ECharts.Types where

import Prelude

import Data.Foreign (Foreign, toForeign)

foreign import data Chart ∷ *

-- | For Eff computation
foreign import data ECHARTS ∷ !

newtype Option = Option Foreign

newtype Grid = Grid Foreign

newtype Tooltip = Tooltip Foreign

newtype Legend = Legend Foreign

newtype XAxis = XAxis Foreign

newtype YAxis = YAxis Foreign

newtype Series = Series Foreign

newtype Title = Title Foreign

data TooltipTrigger
  = ItemTrigger
  | AxisTrigger

tooltipTriggerToForeign ∷ TooltipTrigger → Foreign
tooltipTriggerToForeign = toForeign <<< case _ of
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

data Orient
  = Vertical
  | Horizontal

orientToForeign ∷ Orient → Foreign
orientToForeign = toForeign <<< case _ of
  Vertical → "vertical"
  Horizontal → "horizontal"

newtype Item = Item Foreign

data AxisType
  = Category
  | Value
  | Time
  | Log

axisTypeToForeign ∷ AxisType → Foreign
axisTypeToForeign = toForeign <<< case _ of
  Category → "category"
  Value → "value"
  Time → "time"
  Log → "log"

newtype AxisTick = AxisTick Foreign

newtype AxisLabel = AxisLabel Foreign

newtype PieSeries = PieSeries Foreign

newtype LineSeries = LineSeries Foreign

newtype BarSeries = BarSeries Foreign

newtype ScatterSeries = ScatterSeries Foreign

newtype EffectScatterSeries = EffectScatterSeries Foreign

newtype RadarSeries = RadarSeries Foreign

newtype TreeMapSeries = TreeMapSeries Foreign

newtype BoxPlotSeries = BoxPlotSeries Foreign

newtype CandlestickSeries = CandlestickSeries Foreign

newtype HeatMapSeries = HeatMapSeries Foreign

newtype MapSeries = MapSeries Foreign

newtype ParallelSeries = ParallelSeries Foreign

newtype LinesSeries = LinesSeries Foreign

newtype GraphSeries = GraphSeries Foreign

newtype SankeySeries = SankeySeries Foreign

newtype FunnelSeries = FunnelSeries Foreign

newtype GaugeSeries = GaugeSeries Foreign

newtype SymbolSize = SymbolSize Foreign

numSymbolSize ∷ Number → SymbolSize
numSymbolSize = SymbolSize <<< toForeign

newtype AxisPointer = AxisPointer Foreign

data Symbol
  = Circle
  | Rect
  | RoundRect
  | Triangle
  | Diamond
  | Pin
  | Arrow
  | EmptyCircle

symbolToForeign ∷ Symbol → Foreign
symbolToForeign = toForeign <<< case _ of
  Circle → "circle"
  Rect → "rect"
  RoundRect → "roundRect"
  Triangle → "triangle"
  Diamond → "diamond"
  Pin → "pin"
  Arrow → "arrow"
  EmptyCircle → "emptyCircle"

newtype ItemStyle = ItemStyle Foreign

newtype LineStyle = LineStyle Foreign

newtype AreaStyle = AreaStyle Foreign

newtype Point = Point { x ∷ PixelOrPercent, y ∷ PixelOrPercent }
pointToForeign ∷ Point → Foreign
pointToForeign (Point {x, y}) =
  toForeign [ pixelOrPercentToForeign x, pixelOrPercentToForeign y ]

newtype Radius = Radius { start ∷ PixelOrPercent, end ∷ PixelOrPercent }
radiusToForeign ∷ Radius → Foreign
radiusToForeign (Radius {start, end}) =
  toForeign [ pixelOrPercentToForeign start, pixelOrPercentToForeign end ]

numItem ∷ Number → Item
numItem = Item <<< toForeign

strItem ∷ String → Item
strItem = Item <<< toForeign

data PointerType
  = LinePointer
  | CrossPointer
  | ShadowPointer

pointerTypeToForeign ∷ PointerType → Foreign
pointerTypeToForeign = toForeign <<< case _ of
  LinePointer → "line"
  CrossPointer → "cross"
  ShadowPointer → "shadow"

data LineType
  = SolidLine
  | DashedLine
  | DottedLine

lineTypeToForeign ∷ LineType → Foreign
lineTypeToForeign = toForeign <<< case _ of
  SolidLine → "solid"
  DashedLine → "dashed"
  DottedLine → "dotted"

pairItem ∷ Number → Number → Item
pairItem x y = Item $ toForeign [ x, y ]

newtype Formatter = Formatter Foreign

type FormatterInput =
  { componentType ∷ String
  , seriesIndex ∷ Int
  , seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , "data" ∷ Item -- ???
  , value ∷ Number
  , color ∷ String
  , percent ∷ Number
  }

newtype SplitLine = SplitLine Foreign

data SelectedMode
  = Single
  | Multiple
  | Disabled

selectedModeToForeign ∷ SelectedMode → Foreign
selectedModeToForeign = case _ of
  Single → toForeign "single"
  Multiple → toForeign "multiple"
  Disabled → toForeign false

newtype Label = Label Foreign

newtype LabelInner = LabelInner Foreign

newtype Items = Items Foreign

data HorizontalPosition
  = LeftHP
  | RightHP
  | CenterHP

horizontalPositionToForeign ∷ HorizontalPosition → Foreign
horizontalPositionToForeign = toForeign <<< case _ of
  LeftHP → "left"
  RightHP → "right"
  CenterHP → "center"


newtype Brush = Brush Foreign

newtype BrushToolbox = BrushToolbox Foreign

newtype Toolbox = Toolbox Foreign

newtype Feature = Feature Foreign

newtype MagicType = MagicType Foreign
