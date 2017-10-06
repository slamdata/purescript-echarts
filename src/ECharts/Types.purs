module ECharts.Types where

import Prelude

import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign, toForeign)
import Data.Variant as V
import Data.StrMap as SM

foreign import data Chart ∷ Type

-- | For Eff computation
foreign import data ECHARTS ∷ Effect

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

data Orient
  = Vertical
  | Horizontal

orientToForeign ∷ Orient → Foreign
orientToForeign = toForeign <<< case _ of
  Vertical → "vertical"
  Horizontal → "horizontal"

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

data Symbol
  = Circle
  | Rect
  | RoundRect
  | Triangle
  | Diamond
  | Pin
  | Arrow
  | EmptyCircle
  | None

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
  None → "none"


newtype Point = Point { x ∷ PixelOrPercent, y ∷ PixelOrPercent }
pointToForeign ∷ Point → Foreign
pointToForeign (Point {x, y}) =
  toForeign [ pixelOrPercentToForeign x, pixelOrPercentToForeign y ]

newtype Radius = Radius { start ∷ PixelOrPercent, end ∷ PixelOrPercent }
radiusToForeign ∷ Radius → Foreign
radiusToForeign (Radius {start, end}) =
  toForeign [ pixelOrPercentToForeign start, pixelOrPercentToForeign end ]

newtype SingleValueRadius = SingleValueRadius PixelOrPercent
singleValueRadiusToForeign ∷ SingleValueRadius → Foreign
singleValueRadiusToForeign (SingleValueRadius r) = pixelOrPercentToForeign r

numItem ∷ Number → Item
numItem = Item <<< toForeign

strItem ∷ String → Item
strItem = Item <<< toForeign

numArrItem ∷ Array Number → Item
numArrItem = Item <<< toForeign

strArrItem ∷ Array String → Item
strArrItem = Item <<< toForeign

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

type FormatterInput =
  { componentType ∷ String
  , seriesIndex ∷ Int
  , seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , "data" ∷ Item -- ???
  , value ∷ Foreign
  , color ∷ String
  , percent ∷ Number
  , dataType ∷ String
  }

type FormatterInputArrayValue =
  { componentType ∷ String
  , seriesIndex ∷ Int
  , seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , "data" ∷ Item
  , value ∷ Array Foreign
  , color ∷ String
  , percent ∷ Number
  }

data SelectedMode
  = Single
  | Multiple
  | Disabled

selectedModeToForeign ∷ SelectedMode → Foreign
selectedModeToForeign = case _ of
  Single → toForeign "single"
  Multiple → toForeign "multiple"
  Disabled → toForeign false

data HorizontalPosition
  = LeftHP
  | RightHP
  | CenterHP

horizontalPositionToForeign ∷ HorizontalPosition → Foreign
horizontalPositionToForeign = toForeign <<< case _ of
  LeftHP → "left"
  RightHP → "right"
  CenterHP → "center"

newtype Item = Item Foreign
newtype Coord = Coord Foreign

coord ∷ Number → Number → Coord
coord x y = Coord $ toForeign [ x, y ]

type LegendEventR =
  { name ∷ String
  , selected ∷ SM.StrMap Boolean
  }

type DataRangeEventR =
  { visualMapId ∷ String
  , selected ∷ Array Number
  }

type ClickEventR =
  { seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , seriesIndex ∷ Int
  }

type BrushEventAreaR =
  { brushType ∷ String
  , panelId ∷ String
  , coordRange ∷ Array Foreign
  , xAxisData ∷ Array String
  , yAxisData ∷ Array String
  , gridIndex ∷ Int
  }

type BrushEventR =
  { areas ∷ Array BrushEventAreaR
  , titleTexts ∷ Array String
  }

type EChartsEventR =
  ( click ∷ ClickEventR
  , dblclick ∷ Foreign
  , mousedown ∷ Foreign
  , mousemove ∷ Foreign
  , mouseup ∷ Foreign
  , mouseover ∷ Foreign
  , mouseout ∷ Foreign
  , legendselectchanged ∷ LegendEventR
  , legendselected ∷ LegendEventR
  , legendunselected ∷ LegendEventR
  , datazoom ∷ Foreign
  , datarangeselected ∷ DataRangeEventR
  , timelinechanged ∷ Foreign
  , timelineplaychanged ∷ Foreign
  , restore ∷ Foreign
  , dataviewchanged ∷ Foreign
  , magictypechanged ∷ Foreign
  -- This is not absolutely accurate, but working with pieselectchanged
  -- is too painful
  , pieselectchanged ∷ ClickEventR
  , pieselected ∷ ClickEventR
  , pieunselected ∷ ClickEventR
  , mapselectchanged ∷ Foreign
  , mapselected ∷ Foreign
  , mapunselected ∷ Foreign
  , axisareaselected ∷ Foreign
  , focusNodeAdjancency ∷ Foreign
  , unfocusNodeAdjacency ∷ Foreign
  , brush ∷ BrushEventR
  , brushselected ∷ BrushEventR
  , pieSelect ∷ Foreign
  , pieUnSelect ∷ Foreign
  )

type EChartsEvent = V.Variant EChartsEventR
newtype Option = Option Foreign
