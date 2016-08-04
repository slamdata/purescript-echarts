## Module ECharts.Types

#### `Chart`

``` purescript
data Chart :: *
```

#### `ECHARTS`

``` purescript
data ECHARTS :: !
```

For Eff computation

#### `TooltipTrigger`

``` purescript
data TooltipTrigger
  = ItemTrigger
  | AxisTrigger
```

#### `tooltipTriggerToForeign`

``` purescript
tooltipTriggerToForeign :: TooltipTrigger -> Foreign
```

#### `PixelOrPercent`

``` purescript
data PixelOrPercent
  = Pixel Int
  | Percent Number
```

#### `pixelOrPercentToForeign`

``` purescript
pixelOrPercentToForeign :: PixelOrPercent -> Foreign
```

#### `Orient`

``` purescript
data Orient
  = Vertical
  | Horizontal
```

#### `orientToForeign`

``` purescript
orientToForeign :: Orient -> Foreign
```

#### `AxisType`

``` purescript
data AxisType
  = Category
  | Value
  | Time
  | Log
```

#### `axisTypeToForeign`

``` purescript
axisTypeToForeign :: AxisType -> Foreign
```

#### `Symbol`

``` purescript
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
```

#### `symbolToForeign`

``` purescript
symbolToForeign :: Symbol -> Foreign
```

#### `Point`

``` purescript
newtype Point
  = Point { x :: PixelOrPercent, y :: PixelOrPercent }
```

#### `pointToForeign`

``` purescript
pointToForeign :: Point -> Foreign
```

#### `Radius`

``` purescript
newtype Radius
  = Radius { start :: PixelOrPercent, end :: PixelOrPercent }
```

#### `radiusToForeign`

``` purescript
radiusToForeign :: Radius -> Foreign
```

#### `numItem`

``` purescript
numItem :: Number -> Item
```

#### `strItem`

``` purescript
strItem :: String -> Item
```

#### `PointerType`

``` purescript
data PointerType
  = LinePointer
  | CrossPointer
  | ShadowPointer
```

#### `pointerTypeToForeign`

``` purescript
pointerTypeToForeign :: PointerType -> Foreign
```

#### `LineType`

``` purescript
data LineType
  = SolidLine
  | DashedLine
  | DottedLine
```

#### `lineTypeToForeign`

``` purescript
lineTypeToForeign :: LineType -> Foreign
```

#### `pairItem`

``` purescript
pairItem :: Number -> Number -> Item
```

#### `FormatterInput`

``` purescript
type FormatterInput = { componentType :: String, seriesIndex :: Int, seriesName :: String, name :: String, dataIndex :: Int, data :: Item, value :: Number, color :: String, percent :: Number }
```

#### `SelectedMode`

``` purescript
data SelectedMode
  = Single
  | Multiple
  | Disabled
```

#### `selectedModeToForeign`

``` purescript
selectedModeToForeign :: SelectedMode -> Foreign
```

#### `HorizontalPosition`

``` purescript
data HorizontalPosition
  = LeftHP
  | RightHP
  | CenterHP
```

#### `horizontalPositionToForeign`

``` purescript
horizontalPositionToForeign :: HorizontalPosition -> Foreign
```

#### `Item`

``` purescript
newtype Item
  = Item Foreign
```


