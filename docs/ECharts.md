## Module ECharts


### Re-exported from ECharts.AddData:

#### `AdditionalDataRec`

``` purescript
type AdditionalDataRec = { idx :: Number, datum :: ItemData, isHead :: Boolean, dataGrow :: Boolean, additionalData :: Maybe String }
```

#### `AdditionalData`

``` purescript
newtype AdditionalData
  = AdditionalData AdditionalDataRec
```

##### Instances
``` purescript
EncodeJson AdditionalData
```

#### `addData`

``` purescript
addData :: forall e. AdditionalData -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

### Re-exported from ECharts.Axis:

#### `PolarType`

``` purescript
data PolarType
  = PolarPolygon
  | PolarCircle
```

##### Instances
``` purescript
EncodeJson PolarType
DecodeJson PolarType
```

#### `PolarRec`

``` purescript
type PolarRec = { center :: Maybe (Tuple PercentOrPixel PercentOrPixel), radius :: Maybe PercentOrPixel, startAngle :: Maybe Number, splitNumber :: Maybe Number, name :: Maybe PolarName, boundaryGap :: Maybe (Tuple Number Number), scale :: Maybe Boolean, axisLine :: Maybe AxisLine, axisLabel :: Maybe AxisLabel, splitLine :: Maybe AxisSplitLine, splitArea :: Maybe AxisSplitArea, type :: Maybe PolarType, indicator :: Maybe (Array Indicator) }
```

#### `PolarNameRec`

``` purescript
type PolarNameRec = { show :: Maybe Boolean, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle }
```

#### `PolarName`

``` purescript
newtype PolarName
  = PolarName PolarNameRec
```

##### Instances
``` purescript
EncodeJson PolarName
DecodeJson PolarName
```

#### `Polar`

``` purescript
newtype Polar
  = Polar PolarRec
```

##### Instances
``` purescript
EncodeJson Polar
DecodeJson Polar
```

#### `IndicatorRec`

``` purescript
type IndicatorRec = { text :: Maybe String, min :: Maybe Number, max :: Maybe Number, axisLabel :: Maybe AxisLabel }
```

#### `Indicator`

``` purescript
newtype Indicator
  = Indicator IndicatorRec
```

##### Instances
``` purescript
EncodeJson Indicator
DecodeJson Indicator
```

#### `CustomAxisDataRec`

``` purescript
type CustomAxisDataRec = { value :: String, textStyle :: TextStyle }
```

#### `Axises`

``` purescript
data Axises
  = OneAxis Axis
  | TwoAxises Axis Axis
```

##### Instances
``` purescript
EncodeJson Axises
DecodeJson Axises
```

#### `AxisType`

``` purescript
data AxisType
  = CategoryAxis
  | ValueAxis
  | TimeAxis
```

##### Instances
``` purescript
EncodeJson AxisType
DecodeJson AxisType
```

#### `AxisTickRec`

``` purescript
type AxisTickRec = { show :: Maybe Boolean, splitNumber :: Maybe Number, length :: Maybe Number, lineStyle :: Maybe LineStyle, interval :: Maybe Interval, onGap :: Maybe Boolean, inside :: Maybe Boolean }
```

#### `AxisTick`

``` purescript
newtype AxisTick
  = AxisTick AxisTickRec
```

##### Instances
``` purescript
EncodeJson AxisTick
DecodeJson AxisTick
```

#### `AxisSplitLineRec`

``` purescript
type AxisSplitLineRec = { show :: Maybe Boolean, onGap :: Maybe Boolean, lineStyle :: Maybe LineStyle }
```

#### `AxisSplitLine`

``` purescript
newtype AxisSplitLine
  = AxisSplitLine AxisSplitLineRec
```

##### Instances
``` purescript
EncodeJson AxisSplitLine
DecodeJson AxisSplitLine
```

#### `AxisSplitAreaRec`

``` purescript
type AxisSplitAreaRec = { show :: Maybe Boolean, onGap :: Maybe Boolean, areaStyle :: Maybe AreaStyle }
```

#### `AxisSplitArea`

``` purescript
newtype AxisSplitArea
  = AxisSplitArea AxisSplitAreaRec
```

##### Instances
``` purescript
EncodeJson AxisSplitArea
DecodeJson AxisSplitArea
```

#### `AxisRec`

``` purescript
type AxisRec = { type :: Maybe AxisType, show :: Maybe Boolean, position :: Maybe AxisPosition, name :: Maybe String, nameLocation :: Maybe AxisNameLocation, nameTextStyle :: Maybe TextStyle, boundaryGap :: Maybe AxisBoundaryGap, min :: Maybe Number, max :: Maybe Number, scale :: Maybe Boolean, splitNumber :: Maybe Number, axisLine :: Maybe AxisLine, axisTick :: Maybe AxisTick, axisLabel :: Maybe AxisLabel, splitLine :: Maybe AxisSplitLine, splitArea :: Maybe AxisSplitArea, data :: Maybe (Array AxisData) }
```

#### `AxisPosition`

``` purescript
data AxisPosition
  = LeftAxis
  | RightAxis
  | TopAxis
  | BottomAxis
```

##### Instances
``` purescript
EncodeJson AxisPosition
DecodeJson AxisPosition
```

#### `AxisNameLocation`

``` purescript
data AxisNameLocation
  = Start
  | End
```

##### Instances
``` purescript
EncodeJson AxisNameLocation
DecodeJson AxisNameLocation
```

#### `AxisLineStyleRec`

``` purescript
type AxisLineStyleRec = { color :: Maybe Color, width :: Maybe Number }
```

#### `AxisLineStyle`

``` purescript
newtype AxisLineStyle
  = AxisLineStyle AxisLineStyleRec
```

##### Instances
``` purescript
EncodeJson AxisLineStyle
DecodeJson AxisLineStyle
```

#### `AxisLineRec`

``` purescript
type AxisLineRec = { show :: Maybe Boolean, onZero :: Maybe Boolean, lineStyle :: Maybe AxisLineStyle }
```

#### `AxisLine`

``` purescript
newtype AxisLine
  = AxisLine AxisLineRec
```

##### Instances
``` purescript
EncodeJson AxisLine
DecodeJson AxisLine
```

#### `AxisLabelRec`

``` purescript
type AxisLabelRec = { show :: Maybe Boolean, interval :: Maybe Interval, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle, rotate :: Maybe Number, margin :: Maybe Number, clickable :: Maybe Boolean }
```

#### `AxisLabel`

``` purescript
newtype AxisLabel
  = AxisLabel AxisLabelRec
```

##### Instances
``` purescript
EncodeJson AxisLabel
DecodeJson AxisLabel
```

#### `AxisData`

``` purescript
data AxisData
  = CommonAxisData String
  | CustomAxisData CustomAxisDataRec
```

##### Instances
``` purescript
EncodeJson AxisData
DecodeJson AxisData
```

#### `AxisBoundaryGap`

``` purescript
data AxisBoundaryGap
  = CatBoundaryGap Boolean
  | ValueBoundaryGap Number Number
```

##### Instances
``` purescript
EncodeJson AxisBoundaryGap
DecodeJson AxisBoundaryGap
```

#### `Axis`

``` purescript
newtype Axis
  = Axis AxisRec
```

##### Instances
``` purescript
EncodeJson Axis
DecodeJson Axis
```

#### `polarNameDefault`

``` purescript
polarNameDefault :: PolarNameRec
```

#### `polarDefault`

``` purescript
polarDefault :: PolarRec
```

#### `indicatorDefault`

``` purescript
indicatorDefault :: IndicatorRec
```

#### `axisTickDefault`

``` purescript
axisTickDefault :: AxisTickRec
```

#### `axisSplitLineDefault`

``` purescript
axisSplitLineDefault :: AxisSplitLineRec
```

#### `axisSplitAreaDefault`

``` purescript
axisSplitAreaDefault :: AxisSplitAreaRec
```

#### `axisLineStyleDefault`

``` purescript
axisLineStyleDefault :: AxisLineStyleRec
```

#### `axisLineDefault`

``` purescript
axisLineDefault :: AxisLineRec
```

#### `axisLabelDefault`

``` purescript
axisLabelDefault :: AxisLabelRec
```

#### `axisDefault`

``` purescript
axisDefault :: AxisRec
```

### Re-exported from ECharts.Chart:

#### `ZRender`

``` purescript
data ZRender :: *
```

#### `Theme`

``` purescript
data Theme
  = ThemeName String
  | ThemeConfig Json
```

##### Instances
``` purescript
EncodeJson Theme
```

#### `EChart`

``` purescript
data EChart :: *
```

#### `setTheme`

``` purescript
setTheme :: forall e. Theme -> EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) EChart
```

#### `resize`

``` purescript
resize :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `refresh`

``` purescript
refresh :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `init`

``` purescript
init :: forall e. Maybe Theme -> HTMLElement -> Eff (dom :: DOM, echarts :: ECHARTS | e) EChart
```

#### `getZRender`

``` purescript
getZRender :: forall e. EChart -> Eff e ZRender
```

#### `dispose`

``` purescript
dispose :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `clear`

``` purescript
clear :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

### Re-exported from ECharts.Color:

#### `LinearGradientInput`

``` purescript
type LinearGradientInput = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number, s0 :: Number, sc0 :: String, s1 :: Number, sc1 :: String }
```

#### `LinearGradient`

``` purescript
data LinearGradient :: *
```

#### `ColorFuncParamRec`

``` purescript
type ColorFuncParamRec = { seriesIndex :: Number, series :: String, dataIndex :: Number, data :: { value :: ItemValue, name :: String } }
```

#### `ColorFuncParam`

``` purescript
newtype ColorFuncParam
  = ColorFuncParam ColorFuncParamRec
```

#### `Color`

``` purescript
type Color = String
```

#### `CalculableColor`

``` purescript
data CalculableColor
  = SimpleColor Color
  | ColorFunc (String -> Color)
  | GradientColor LinearGradient
```

##### Instances
``` purescript
EncodeJson CalculableColor
DecodeJson CalculableColor
```

#### `linearGradientInputDefault`

``` purescript
linearGradientInputDefault :: LinearGradientInput
```

### Re-exported from ECharts.Common:

#### `Sort`

``` purescript
data Sort
  = NoSort
  | Asc
  | Desc
```

##### Instances
``` purescript
EncodeJson Sort
DecodeJson Sort
```

#### `SelectedMode`

``` purescript
data SelectedMode
  = SelModeSingle
  | SelModeMultiple
  | SelModeFalse
```

##### Instances
``` purescript
EncodeJson SelectedMode
DecodeJson SelectedMode
```

#### `RsRec`

``` purescript
type RsRec = { inner :: PercentOrPixel, outer :: PercentOrPixel }
```

#### `RoseType`

``` purescript
data RoseType
  = RTRadius
  | RTArea
```

##### Instances
``` purescript
EncodeJson RoseType
DecodeJson RoseType
```

#### `Roam`

``` purescript
data Roam
  = Enable
  | Disable
  | Scale
  | Move
```

##### Instances
``` purescript
EncodeJson Roam
DecodeJson Roam
```

#### `Radius`

``` purescript
data Radius
  = R PercentOrPixel
  | Rs RsRec
```

##### Instances
``` purescript
EncodeJson Radius
DecodeJson Radius
```

#### `PercentOrPixel`

``` purescript
data PercentOrPixel
  = Percent Number
  | Pixel Number
```

##### Instances
``` purescript
EncodeJson PercentOrPixel
DecodeJson PercentOrPixel
```

#### `MinMaxRec`

``` purescript
type MinMaxRec = { min :: Number, max :: Number }
```

#### `MinMax`

``` purescript
newtype MinMax
  = MinMax MinMaxRec
```

##### Instances
``` purescript
EncodeJson MinMax
DecodeJson MinMax
```

#### `MapValueCalculation`

``` purescript
data MapValueCalculation
  = SumCalculation
  | AverageCalculation
```

##### Instances
``` purescript
EncodeJson MapValueCalculation
DecodeJson MapValueCalculation
```

#### `Interval`

``` purescript
data Interval
  = Auto
  | Custom Number
```

##### Instances
``` purescript
EncodeJson Interval
DecodeJson Interval
```

#### `GeoCoord`

``` purescript
type GeoCoord = StrMap (Tuple Number Number)
```

#### `Corner`

``` purescript
data Corner a
  = AllCorners a
  | Corners a a a a
```

##### Instances
``` purescript
(EncodeJson a) => EncodeJson (Corner a)
(DecodeJson a) => DecodeJson (Corner a)
```

#### `Center`

``` purescript
type Center = Tuple PercentOrPixel PercentOrPixel
```

### Re-exported from ECharts.Connect:

#### `Connection`

``` purescript
newtype Connection
```

#### `connect`

``` purescript
connect :: forall e. EChart -> EChart -> Eff (echarts :: ECHARTS | e) Connection
```

### Re-exported from ECharts.Coords:

#### `YPos`

``` purescript
data YPos
  = YTop
  | YBottom
  | YCenter
  | Y Number
```

##### Instances
``` purescript
EncodeJson YPos
DecodeJson YPos
```

#### `XPos`

``` purescript
data XPos
  = XLeft
  | XRight
  | XCenter
  | X Number
```

##### Instances
``` purescript
EncodeJson XPos
DecodeJson XPos
```

#### `Orient`

``` purescript
data Orient
  = Horizontal
  | Vertical
```

##### Instances
``` purescript
EncodeJson Orient
DecodeJson Orient
```

#### `LocationRec`

``` purescript
type LocationRec = { x :: Maybe XPos, y :: Maybe YPos }
```

#### `Location`

``` purescript
newtype Location
  = Location LocationRec
```

##### Instances
``` purescript
EncodeJson Location
DecodeJson Location
```

#### `LabelPosition`

``` purescript
data LabelPosition
  = LPOuter
  | LPInner
  | LPTop
  | LPRight
  | LPLeft
  | LPBottom
  | LPInside
  | LPInsideLeft
  | LPInsideRight
  | LPInsideTop
  | LPInsideBottom
```

##### Instances
``` purescript
EncodeJson LabelPosition
DecodeJson LabelPosition
```

#### `HorizontalAlign`

``` purescript
data HorizontalAlign
  = HAlignLeft
  | HAlignRight
  | HAlignCenter
```

##### Instances
``` purescript
EncodeJson HorizontalAlign
DecodeJson HorizontalAlign
```

### Re-exported from ECharts.DataRange:

#### `DataRangeRec`

``` purescript
type DataRangeRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemWidth :: Maybe Number, itemHeight :: Maybe Number, min :: Maybe Number, max :: Maybe Number, precision :: Maybe Number, splitNumber :: Maybe Number, selectedMode :: Maybe SelectedMode, calculable :: Maybe Boolean, hoverLink :: Maybe Boolean, realtime :: Maybe Boolean, color :: Maybe (Array Color), formatter :: Maybe Formatter, text :: Maybe (Tuple String String), textStyle :: Maybe TextStyle }
```

#### `DataRange`

``` purescript
newtype DataRange
  = DataRange DataRangeRec
```

##### Instances
``` purescript
EncodeJson DataRange
DecodeJson DataRange
```

#### `dataRangeDefault`

``` purescript
dataRangeDefault :: DataRangeRec
```

### Re-exported from ECharts.DataZoom:

#### `DataZoomRec`

``` purescript
type DataZoomRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, width :: Maybe Number, height :: Maybe Number, backgroundColor :: Maybe Color, dataBackgroundColor :: Maybe Color, fillerColor :: Maybe Color, handleColor :: Maybe Color, xAxisIndex :: Maybe (Array Number), yAxisIndex :: Maybe (Array Number), start :: Maybe Number, end :: Maybe Number, showDetail :: Maybe Boolean, realtime :: Maybe Boolean, zoomlock :: Maybe Boolean }
```

#### `DataZoom`

``` purescript
newtype DataZoom
  = DataZoom DataZoomRec
```

##### Instances
``` purescript
EncodeJson DataZoom
DecodeJson DataZoom
```

#### `dataZoomDefault`

``` purescript
dataZoomDefault :: DataZoomRec
```

### Re-exported from ECharts.Effects:

#### `ECHARTS`

``` purescript
data ECHARTS :: !
```

### Re-exported from ECharts.Events:

#### `Sub`

``` purescript
newtype Sub
```

#### `EventType`

``` purescript
data EventType
  = RefreshEvent
  | RestoreEvent
  | ResizeEvent
  | ClickEvent
  | DoubleClickEvent
  | HoverEvent
  | DataChangedEvent
  | DataZoomEvent
  | DataRangeEvent
  | DataRangeHoverLinkEvent
  | LegendSelectedEvent
  | LegendHoverLinkEvent
  | MapSelectedEvent
  | PieSelectedEvent
  | DataViewChangedEvent
  | MapRoamEvent
  | MagicTypeChangedEvent
```

#### `EventParam`

``` purescript
type EventParam = Json
```

#### `listen`

``` purescript
listen :: forall e. EventType -> (EventParam -> Eff (echarts :: ECHARTS | e) Unit) -> EChart -> Eff (echarts :: ECHARTS | e) Sub
```

### Re-exported from ECharts.Formatter:

#### `Formatter`

``` purescript
data Formatter
  = Template String
  | FormatFunc (Array FormatParams -> String)
  | StringFormatFunc (String -> String)
  | NumberFormatFunc (Number -> String)
```

##### Instances
``` purescript
EncodeJson Formatter
DecodeJson Formatter
```

#### `FormatParams`

``` purescript
type FormatParams = Json
```

### Re-exported from ECharts.Grid:

#### `GridRec`

``` purescript
type GridRec = { x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Number }
```

#### `Grid`

``` purescript
newtype Grid
  = Grid GridRec
```

##### Instances
``` purescript
EncodeJson Grid
DecodeJson Grid
```

#### `gridDefault`

``` purescript
gridDefault :: GridRec
```

### Re-exported from ECharts.Image:

#### `ImgType`

``` purescript
data ImgType
  = PNG
  | JPEG
```

##### Instances
``` purescript
EncodeJson ImgType
DecodeJson ImgType
```

#### `getImage`

``` purescript
getImage :: forall e. ImgType -> EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Node
```

#### `getDataURL`

``` purescript
getDataURL :: forall e. ImgType -> EChart -> Eff (echarts :: ECHARTS | e) String
```

### Re-exported from ECharts.Item.Data:

#### `ItemDataDatRec`

``` purescript
type ItemDataDatRec = { value :: ItemValue, name :: Maybe String, tooltip :: Maybe Tooltip, itemStyle :: Maybe ItemStyle, selected :: Maybe Boolean }
```

#### `ItemData`

``` purescript
data ItemData
  = Value ItemValue
  | Dat ItemDataDatRec
  | Label String
```

##### Instances
``` purescript
EncodeJson ItemData
DecodeJson ItemData
```

#### `dataDefault`

``` purescript
dataDefault :: ItemValue -> ItemDataDatRec
```

### Re-exported from ECharts.Item.Value:

#### `XYRRec`

``` purescript
type XYRRec = { x :: Number, y :: Number, r :: Maybe Number }
```

#### `ItemValue`

``` purescript
data ItemValue
  = None
  | Simple Number
  | Many (Array Number)
  | XYR XYRRec
  | HLOC HLOCRec
```

##### Instances
``` purescript
EncodeJson ItemValue
DecodeJson ItemValue
```

#### `HLOCRec`

``` purescript
type HLOCRec = { h :: Number, l :: Number, o :: Number, c :: Number }
```

### Re-exported from ECharts.Legend:

#### `LegendRec`

``` purescript
type LegendRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemHeight :: Maybe Number, itemWidth :: Maybe Number, textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, selectedMode :: Maybe SelectedMode, selected :: Maybe (StrMap Boolean), data :: Maybe (Array LegendItem) }
```

#### `LegendItemRec`

``` purescript
type LegendItemRec = { icon :: Maybe String, textStyle :: Maybe TextStyle }
```

#### `LegendItem`

``` purescript
data LegendItem
  = LegendItem String LegendItemRec
```

##### Instances
``` purescript
EncodeJson LegendItem
DecodeJson LegendItem
```

#### `Legend`

``` purescript
newtype Legend
  = Legend LegendRec
```

##### Instances
``` purescript
EncodeJson Legend
DecodeJson Legend
```

#### `legendItemDefault`

``` purescript
legendItemDefault :: String -> LegendItem
```

#### `legendDefault`

``` purescript
legendDefault :: LegendRec
```

### Re-exported from ECharts.Loading:

#### `LoadingOptionRec`

``` purescript
type LoadingOptionRec = { text :: Maybe String, x :: Maybe XPos, y :: Maybe YPos, textStyle :: Maybe TextStyle, effect :: Maybe LoadingEffect, effectOption :: Maybe Json, progress :: Maybe Number }
```

#### `LoadingOption`

``` purescript
newtype LoadingOption
  = LoadingOption LoadingOptionRec
```

##### Instances
``` purescript
EncodeJson LoadingOption
```

#### `LoadingEffect`

``` purescript
data LoadingEffect
  = Spin
  | Bar
  | Ring
  | Whirling
  | DynamicLine
  | Bubble
```

##### Instances
``` purescript
EncodeJson LoadingEffect
```

#### `showLoading`

``` purescript
showLoading :: forall e. LoadingOption -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

#### `loadingOptionDefault`

``` purescript
loadingOptionDefault :: LoadingOptionRec
```

#### `hideLoading`

``` purescript
hideLoading :: forall e. EChart -> Eff (echarts :: ECHARTS | e) EChart
```

### Re-exported from ECharts.Mark.Data:

#### `MarkPointDataRec`

``` purescript
type MarkPointDataRec = { name :: Maybe String, value :: Maybe Number, x :: Maybe Number, y :: Maybe Number, xAxis :: Maybe Number, yAxis :: Maybe Number, type :: Maybe String }
```

#### `MarkPointData`

``` purescript
newtype MarkPointData
  = MarkPointData MarkPointDataRec
```

##### Instances
``` purescript
EncodeJson MarkPointData
DecodeJson MarkPointData
```

#### `markPointDataDefault`

``` purescript
markPointDataDefault :: MarkPointDataRec
```

### Re-exported from ECharts.Mark.Effect:

#### `MarkPointEffectRec`

``` purescript
type MarkPointEffectRec = { show :: Maybe Boolean, loop :: Maybe Boolean, period :: Maybe Boolean, scaleSize :: Maybe Boolean, color :: Maybe Color, shadowBlur :: Maybe Number }
```

#### `MarkPointEffect`

``` purescript
newtype MarkPointEffect
  = MarkPointEffect MarkPointEffectRec
```

##### Instances
``` purescript
EncodeJson MarkPointEffect
DecodeJson MarkPointEffect
```

#### `markPointEffectDefault`

``` purescript
markPointEffectDefault :: MarkPointEffectRec
```

### Re-exported from ECharts.Mark.Line:

#### `MarkLineRec`

``` purescript
type MarkLineRec = { symbol :: Maybe (Tuple Symbol Symbol), symbolSize :: Maybe DoubleSymbolSize, symbolRotate :: Maybe (Tuple Number Number), effect :: Maybe MarkPointEffect, geoCoord :: Maybe (Array GeoCoord), data :: Maybe (Array (Tuple MarkPointData MarkPointData)), itemStyle :: Maybe ItemStyle }
```

#### `MarkLine`

``` purescript
newtype MarkLine
  = MarkLine MarkLineRec
```

##### Instances
``` purescript
EncodeJson MarkLine
DecodeJson MarkLine
```

#### `markLineDefault`

``` purescript
markLineDefault :: MarkLineRec
```

#### `delMarkLine`

``` purescript
delMarkLine :: forall e. Number -> String -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

#### `addMarkLine`

``` purescript
addMarkLine :: forall e. MarkLine -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

### Re-exported from ECharts.Mark.Point:

#### `MarkPointRec`

``` purescript
type MarkPointRec = { symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, large :: Maybe Boolean, effect :: Maybe MarkPointEffect, data :: Maybe (Array MarkPointData), geoCoord :: Maybe (StrMap (Tuple Number Number)) }
```

#### `MarkPoint`

``` purescript
newtype MarkPoint
  = MarkPoint MarkPointRec
```

##### Instances
``` purescript
EncodeJson MarkPoint
DecodeJson MarkPoint
```

#### `markPointDefault`

``` purescript
markPointDefault :: MarkPointRec
```

#### `delMarkPoint`

``` purescript
delMarkPoint :: forall e. Number -> String -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

#### `addMarkPoint`

``` purescript
addMarkPoint :: forall e. MarkPoint -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

### Re-exported from ECharts.Options:

#### `OptionRec`

``` purescript
type OptionRec = { backgroundColor :: Maybe Color, color :: Maybe (Array Color), renderAsImage :: Maybe Boolean, calculable :: Maybe Boolean, animation :: Maybe Boolean, timeline :: Maybe Timeline, tooltip :: Maybe Tooltip, toolbox :: Maybe Toolbox, title :: Maybe Title, legend :: Maybe Legend, dataRange :: Maybe DataRange, dataZoom :: Maybe DataZoom, roamController :: Maybe RoamController, grid :: Maybe Grid, xAxis :: Maybe Axises, yAxis :: Maybe Axises, polar :: Maybe (Array Polar), series :: Maybe (Array (Maybe Series)) }
```

#### `Option`

``` purescript
newtype Option
  = Option OptionRec
```

##### Instances
``` purescript
EncodeJson Option
DecodeJson Option
```

#### `setOption`

``` purescript
setOption :: forall e. Option -> Boolean -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```

#### `optionDefault`

``` purescript
optionDefault :: OptionRec
```

### Re-exported from ECharts.RoamController:

#### `RoamControllerRec`

``` purescript
type RoamControllerRec = { show :: Maybe Boolean, x :: Maybe XPos, y :: Maybe YPos, width :: Maybe Number, height :: Maybe Number, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), fillerColor :: Maybe Color, handleColor :: Maybe Color, step :: Maybe Number, mapTypeControl :: Maybe (StrMap Boolean) }
```

#### `RoamController`

``` purescript
newtype RoamController
  = RoamController RoamControllerRec
```

##### Instances
``` purescript
EncodeJson RoamController
DecodeJson RoamController
```

#### `roamControllerDefault`

``` purescript
roamControllerDefault :: RoamControllerRec
```

### Re-exported from ECharts.Series:

#### `UniversalSeriesRec`

``` purescript
type UniversalSeriesRec = { name :: Maybe String, tooltip :: Maybe Tooltip, clickable :: Maybe Boolean, itemStyle :: Maybe ItemStyle, markPoint :: Maybe MarkPoint, markLine :: Maybe MarkLine }
```

#### `Series`

``` purescript
data Series
  = LineSeries { common :: UniversalSeriesRec, lineSeries :: LineSeriesRec }
  | BarSeries { common :: UniversalSeriesRec, barSeries :: BarSeriesRec }
  | ScatterSeries { common :: UniversalSeriesRec, scatterSeries :: ScatterSeriesRec }
  | CandlestickSeries { common :: UniversalSeriesRec, candlestickSeries :: CandlestickSeriesRec }
  | PieSeries { common :: UniversalSeriesRec, pieSeries :: PieSeriesRec }
  | RadarSeries { common :: UniversalSeriesRec, radarSeries :: RadarSeriesRec }
  | ChordSeries { common :: UniversalSeriesRec, chordSeries :: ChordSeriesRec }
  | ForceSeries { common :: UniversalSeriesRec, forceSeries :: ForceSeriesRec }
  | MapSeries { common :: UniversalSeriesRec, mapSeries :: MapSeriesRec }
  | GaugeSeries { common :: UniversalSeriesRec, gaugeSeries :: GaugeSeriesRec }
  | FunnelSeries { common :: UniversalSeriesRec, funnelSeries :: FunnelSeriesRec }
  | EventRiverSeries { common :: UniversalSeriesRec, eventRiverSeries :: EventRiverSeriesRec }
```

##### Instances
``` purescript
EncodeJson Series
DecodeJson Series
```

#### `ScatterSeriesRec`

``` purescript
type ScatterSeriesRec = { data :: Maybe (Array ItemData), xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, large :: Maybe Boolean, largeThreshold :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `RadarSeriesRec`

``` purescript
type RadarSeriesRec = { data :: Maybe (Array ItemData), polarIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, legendHoverLink :: Maybe Boolean }
```

#### `PieSeriesRec`

``` purescript
type PieSeriesRec = { data :: Maybe (Array ItemData), center :: Maybe Center, radius :: Maybe Radius, startAngle :: Maybe Number, minAngle :: Maybe Number, clockWise :: Maybe Boolean, roseType :: Maybe RoseType, selectedOffset :: Maybe Number, selectedMode :: Maybe SelectedMode, legendHoverLink :: Maybe Boolean }
```

#### `MapSeriesRec`

``` purescript
type MapSeriesRec = { data :: Maybe (Array ItemData), selectedMode :: Maybe SelectedMode, mapType :: Maybe String, hoverable :: Maybe Boolean, dataRangeHoverLink :: Maybe Boolean, mapLocation :: Maybe Location, mapValueCalculation :: Maybe MapValueCalculation, mapValuePrecision :: Maybe Number, showLegendSymbol :: Maybe Boolean, roam :: Maybe Roam, scaleLimit :: Maybe MinMax, nameMap :: Maybe (StrMap String), textFixed :: Maybe (StrMap (Tuple Number Number)), geoCoord :: Maybe (StrMap (Tuple Number Number)) }
```

#### `LineSeriesRec`

``` purescript
type LineSeriesRec = { data :: Maybe (Array ItemData), stack :: Maybe String, xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbolRotate :: Maybe Boolean, showAllSymbol :: Maybe Boolean, smooth :: Maybe Boolean, legendHoverLink :: Maybe Boolean }
```

#### `GaugeSeriesRec`

``` purescript
type GaugeSeriesRec = { data :: Maybe (Array ItemData), center :: Maybe (Tuple Number Number), radius :: Maybe Radius, startAngle :: Maybe Number, endAngle :: Maybe Number, min :: Maybe Number, max :: Maybe Number, precision :: Maybe Number, splitNumber :: Maybe Number, axisLine :: Maybe AxisLine, axisTick :: Maybe AxisTick, axisLabel :: Maybe AxisLabel, splitLine :: Maybe SplitLine, title :: Maybe Title, detail :: Maybe GaugeDetail, pointer :: Maybe Pointer, legendHoverLink :: Maybe Boolean }
```

#### `FunnelSeriesRec`

``` purescript
type FunnelSeriesRec = { data :: Maybe (Array ItemData), x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, funnelAlign :: Maybe HorizontalAlign, min :: Maybe Number, max :: Maybe Number, minSize :: Maybe PercentOrPixel, maxSize :: Maybe PercentOrPixel, gap :: Maybe Number, sort :: Maybe Sort, legendHoverLink :: Maybe Boolean }
```

#### `ForceSeriesRec`

``` purescript
type ForceSeriesRec = { categories :: Maybe (Array ForceCategory), nodes :: Maybe (Array Node), links :: Maybe (Array Link), matrix :: Maybe Matrix, center :: Maybe Center, size :: Maybe Number, minRadius :: Maybe Number, maxRadius :: Maybe Number, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, linkSymbol :: Maybe Symbol, linkSymbolSize :: Maybe Symbol, scaling :: Maybe Number, gravity :: Maybe Number, draggable :: Maybe Number, large :: Maybe Boolean, useWorker :: Maybe Boolean, steps :: Maybe Number, ribbonType :: Maybe Boolean }
```

#### `EventRiverSeriesRec`

``` purescript
type EventRiverSeriesRec = { eventList :: Maybe (Array OneEvent), xAxisIndex :: Maybe Number, weight :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `ChordSeriesRec`

``` purescript
type ChordSeriesRec = { nodes :: Maybe (Array Node), categories :: Maybe (Array ForceCategory), links :: Maybe (Array Link), matrix :: Maybe Matrix, data :: Maybe (Array ItemData), ribbonType :: Maybe Boolean, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, minRadius :: Maybe Number, maxRadius :: Maybe Number, showScale :: Maybe Boolean, showScaleText :: Maybe Boolean, padding :: Maybe Number, sort :: Maybe Sort, sortSub :: Maybe Sort, clockWise :: Maybe Boolean }
```

#### `CandlestickSeriesRec`

``` purescript
type CandlestickSeriesRec = { data :: Maybe (Array ItemData), xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, barMinHeight :: Maybe Number, barWidth :: Maybe Number, barMaxWidth :: Maybe Number }
```

#### `BarSeriesRec`

``` purescript
type BarSeriesRec = { data :: Maybe (Array ItemData), stack :: Maybe String, xAxisIndex :: Maybe Number, yAxisIndex :: Maybe Number, barGap :: Maybe PercentOrPixel, barCategoryGap :: Maybe PercentOrPixel, barMinHeight :: Maybe Number, barWidth :: Maybe Number, barMaxWidth :: Maybe Number, legendHoverLink :: Maybe Boolean }
```

#### `universalSeriesDefault`

``` purescript
universalSeriesDefault :: UniversalSeriesRec
```

#### `setSeries`

``` purescript
setSeries :: forall e. Array Series -> Boolean -> EChart -> Eff e EChart
```

#### `scatterSeriesDefault`

``` purescript
scatterSeriesDefault :: ScatterSeriesRec
```

#### `radarSeriesDefault`

``` purescript
radarSeriesDefault :: RadarSeriesRec
```

#### `pieSeriesDefault`

``` purescript
pieSeriesDefault :: PieSeriesRec
```

#### `mapSeriesDefault`

``` purescript
mapSeriesDefault :: MapSeriesRec
```

#### `lineSeriesDefault`

``` purescript
lineSeriesDefault :: LineSeriesRec
```

#### `gaugeSeriesDefault`

``` purescript
gaugeSeriesDefault :: GaugeSeriesRec
```

#### `funnelSeriesDefault`

``` purescript
funnelSeriesDefault :: FunnelSeriesRec
```

#### `forceSeriesDefault`

``` purescript
forceSeriesDefault :: ForceSeriesRec
```

#### `eventRiverSeriesDefault`

``` purescript
eventRiverSeriesDefault :: EventRiverSeriesRec
```

#### `chordSeriesDefault`

``` purescript
chordSeriesDefault :: ChordSeriesRec
```

#### `candlestickSeriesDefault`

``` purescript
candlestickSeriesDefault :: CandlestickSeriesRec
```

#### `barSeriesDefault`

``` purescript
barSeriesDefault :: BarSeriesRec
```

### Re-exported from ECharts.Series.EventRiver:

#### `OneEventRec`

``` purescript
type OneEventRec = { name :: Maybe String, weight :: Maybe Number, evolution :: Maybe (Array Evolution) }
```

#### `OneEvent`

``` purescript
newtype OneEvent
  = OneEvent OneEventRec
```

##### Instances
``` purescript
EncodeJson OneEvent
DecodeJson OneEvent
```

#### `EvolutionRec`

``` purescript
type EvolutionRec = { time :: DateTime, value :: Number, detail :: Maybe EvolutionDetail }
```

#### `EvolutionDetailRec`

``` purescript
type EvolutionDetailRec = { link :: Maybe String, text :: Maybe String, img :: Maybe String }
```

#### `EvolutionDetail`

``` purescript
newtype EvolutionDetail
  = EvolutionDetail EvolutionDetailRec
```

##### Instances
``` purescript
EncodeJson EvolutionDetail
DecodeJson EvolutionDetail
```

#### `Evolution`

``` purescript
newtype Evolution
  = Evolution EvolutionRec
```

##### Instances
``` purescript
EncodeJson Evolution
DecodeJson Evolution
```

#### `oneEventDefault`

``` purescript
oneEventDefault :: OneEventRec
```

#### `evolutionDetailDefault`

``` purescript
evolutionDetailDefault :: EvolutionDetailRec
```

### Re-exported from ECharts.Series.Force:

#### `NodeRec`

``` purescript
type NodeRec = { name :: Maybe String, label :: Maybe String, value :: Number, ignore :: Maybe Boolean, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, itemStyle :: Maybe ItemStyle, initial :: Maybe (Tuple Number Number), fixX :: Maybe Boolean, fixY :: Maybe Boolean, draggable :: Maybe Boolean, category :: Maybe Number }
```

#### `Node`

``` purescript
newtype Node
  = Node NodeRec
```

##### Instances
``` purescript
EncodeJson Node
DecodeJson Node
```

#### `Matrix`

``` purescript
type Matrix = Array (Array Number)
```

#### `LinkRec`

``` purescript
type LinkRec = { source :: LinkEnd, target :: LinkEnd, weight :: Number, itemStyle :: Maybe ItemStyle }
```

#### `LinkEnd`

``` purescript
data LinkEnd
  = Name String
  | Index Number
```

##### Instances
``` purescript
EncodeJson LinkEnd
DecodeJson LinkEnd
```

#### `Link`

``` purescript
newtype Link
  = Link LinkRec
```

##### Instances
``` purescript
EncodeJson Link
DecodeJson Link
```

#### `ForceCategoryRec`

``` purescript
type ForceCategoryRec = { name :: Maybe String, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, itemStyle :: Maybe ItemStyle }
```

#### `ForceCategory`

``` purescript
newtype ForceCategory
  = ForceCategory ForceCategoryRec
```

##### Instances
``` purescript
EncodeJson ForceCategory
DecodeJson ForceCategory
```

#### `nodeDefault`

``` purescript
nodeDefault :: Number -> NodeRec
```

#### `forceCategoryDefault`

``` purescript
forceCategoryDefault :: ForceCategoryRec
```

### Re-exported from ECharts.Series.Gauge:

#### `SplitLineRec`

``` purescript
type SplitLineRec = { show :: Maybe Boolean, length :: Maybe Number, lineStyle :: Maybe LineStyle }
```

#### `SplitLine`

``` purescript
newtype SplitLine
  = SplitLine SplitLineRec
```

##### Instances
``` purescript
EncodeJson SplitLine
DecodeJson SplitLine
```

#### `PointerRec`

``` purescript
type PointerRec = { length :: Maybe Number, width :: Maybe Number, color :: Maybe Color }
```

#### `Pointer`

``` purescript
newtype Pointer
  = Pointer PointerRec
```

##### Instances
``` purescript
EncodeJson Pointer
DecodeJson Pointer
```

#### `GaugeDetailRec`

``` purescript
type GaugeDetailRec = { show :: Maybe Boolean, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, width :: Maybe Number, height :: Maybe Number, offsetCenter :: Maybe (Tuple PercentOrPixel PercentOrPixel), formatter :: Maybe Formatter, textStyle :: Maybe TextStyle }
```

#### `GaugeDetail`

``` purescript
newtype GaugeDetail
  = GaugeDetail GaugeDetailRec
```

##### Instances
``` purescript
EncodeJson GaugeDetail
DecodeJson GaugeDetail
```

#### `splitLineDefault`

``` purescript
splitLineDefault :: SplitLineRec
```

#### `pointerDefault`

``` purescript
pointerDefault :: PointerRec
```

#### `gaugeDetailDefault`

``` purescript
gaugeDetailDefault :: GaugeDetailRec
```

### Re-exported from ECharts.Style.Area:

#### `AreaStyleRec`

``` purescript
type AreaStyleRec = { color :: Maybe CalculableColor, type :: Maybe String }
```

#### `AreaStyle`

``` purescript
newtype AreaStyle
  = AreaStyle AreaStyleRec
```

##### Instances
``` purescript
EncodeJson AreaStyle
DecodeJson AreaStyle
```

#### `areaStyleDefault`

``` purescript
areaStyleDefault :: AreaStyleRec
```

### Re-exported from ECharts.Style.Checkpoint:

#### `CheckpointStyleRec`

``` purescript
type CheckpointStyleRec = { symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, color :: Maybe Color, borderColor :: Maybe Color, label :: Maybe AxisLabel }
```

#### `CheckpointStyle`

``` purescript
newtype CheckpointStyle
  = CheckpointStyle CheckpointStyleRec
```

##### Instances
``` purescript
EncodeJson CheckpointStyle
DecodeJson CheckpointStyle
```

#### `checkpointStyleDefault`

``` purescript
checkpointStyleDefault :: CheckpointStyleRec
```

### Re-exported from ECharts.Style.Chord:

#### `ChordStyleRec`

``` purescript
type ChordStyleRec = { width :: Maybe Number, color :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color }
```

#### `ChordStyle`

``` purescript
newtype ChordStyle
  = ChordStyle ChordStyleRec
```

##### Instances
``` purescript
EncodeJson ChordStyle
DecodeJson ChordStyle
```

#### `chordStyleDefault`

``` purescript
chordStyleDefault :: ChordStyleRec
```

### Re-exported from ECharts.Style.Item:

#### `ItemStyleRec`

``` purescript
type ItemStyleRec = { normal :: Maybe IStyle, emphasis :: Maybe IStyle }
```

#### `ItemStyle`

``` purescript
newtype ItemStyle
  = ItemStyle ItemStyleRec
```

##### Instances
``` purescript
EncodeJson ItemStyle
DecodeJson ItemStyle
```

#### `ItemLabelRec`

``` purescript
type ItemLabelRec = { show :: Maybe Boolean, position :: Maybe LabelPosition, rotate :: Maybe Boolean, distance :: Maybe Boolean, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle }
```

#### `ItemLabelLineRec`

``` purescript
type ItemLabelLineRec = { show :: Maybe Boolean, length :: Maybe Number, lineStyle :: Maybe LineStyle }
```

#### `ItemLabelLine`

``` purescript
newtype ItemLabelLine
  = ItemLabelLine ItemLabelLineRec
```

##### Instances
``` purescript
EncodeJson ItemLabelLine
DecodeJson ItemLabelLine
```

#### `ItemLabel`

``` purescript
newtype ItemLabel
  = ItemLabel ItemLabelRec
```

##### Instances
``` purescript
EncodeJson ItemLabel
DecodeJson ItemLabel
```

#### `IStyleRec`

``` purescript
type IStyleRec = { color :: Maybe CalculableColor, borderColor :: Maybe Color, borderWidth :: Maybe Number, barBorderColor :: Maybe Color, barBorderRadius :: Maybe (Corner Number), barBorderWidth :: Maybe Number, label :: Maybe ItemLabel, labelLine :: Maybe ItemLabelLine, lineStyle :: Maybe LineStyle, areaStyle :: Maybe AreaStyle, chordStyle :: Maybe ChordStyle, nodeStyle :: Maybe NodeStyle, linkStyle :: Maybe LinkStyle }
```

#### `IStyle`

``` purescript
newtype IStyle
  = IStyle IStyleRec
```

##### Instances
``` purescript
EncodeJson IStyle
DecodeJson IStyle
```

#### `itemStyleDefault`

``` purescript
itemStyleDefault :: ItemStyleRec
```

#### `itemLabelLineDefault`

``` purescript
itemLabelLineDefault :: ItemLabelLineRec
```

#### `itemLabelDefault`

``` purescript
itemLabelDefault :: ItemLabelRec
```

#### `istyleDefault`

``` purescript
istyleDefault :: IStyleRec
```

### Re-exported from ECharts.Style.Line:

#### `LineType`

``` purescript
data LineType
  = Solid
  | Dotted
  | Dashed
```

##### Instances
``` purescript
EncodeJson LineType
DecodeJson LineType
```

#### `LineStyleRec`

``` purescript
type LineStyleRec = { color :: Maybe Color, type :: Maybe LineType, width :: Maybe Number, shadowColor :: Maybe Color, shadowOffsetX :: Maybe Number, shadowOffsetY :: Maybe Number }
```

#### `LineStyle`

``` purescript
newtype LineStyle
  = LineStyle LineStyleRec
```

##### Instances
``` purescript
EncodeJson LineStyle
DecodeJson LineStyle
```

#### `lineStyleDefault`

``` purescript
lineStyleDefault :: LineStyleRec
```

### Re-exported from ECharts.Style.Link:

#### `LinkType`

``` purescript
data LinkType
  = LTCurve
  | LTLine
```

##### Instances
``` purescript
EncodeJson LinkType
DecodeJson LinkType
```

#### `LinkStyleRec`

``` purescript
type LinkStyleRec = { type :: Maybe LinkType, color :: Maybe Color, width :: Maybe Number }
```

#### `LinkStyle`

``` purescript
newtype LinkStyle
  = LinkStyle LinkStyleRec
```

##### Instances
``` purescript
EncodeJson LinkStyle
DecodeJson LinkStyle
```

#### `linkStyleDefault`

``` purescript
linkStyleDefault :: LinkStyleRec
```

### Re-exported from ECharts.Style.Node:

#### `NodeStyleRec`

``` purescript
type NodeStyleRec = { color :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number }
```

#### `NodeStyle`

``` purescript
newtype NodeStyle
  = NodeStyle NodeStyleRec
```

##### Instances
``` purescript
EncodeJson NodeStyle
DecodeJson NodeStyle
```

#### `nodeStyleDefault`

``` purescript
nodeStyleDefault :: NodeStyleRec
```

### Re-exported from ECharts.Style.Text:

#### `TextStyleRec`

``` purescript
type TextStyleRec = { color :: Maybe Color, decoration :: Maybe Decoration, align :: Maybe HorizontalAlign, baseline :: Maybe TextBaseline, fontFamily :: Maybe FontFamily, fontSize :: Maybe Number, fontStyle :: Maybe FontStyle, fontWeight :: Maybe FontWeight }
```

#### `TextStyle`

``` purescript
newtype TextStyle
  = TextStyle TextStyleRec
```

##### Instances
``` purescript
EncodeJson TextStyle
DecodeJson TextStyle
```

#### `TextBaseline`

``` purescript
data TextBaseline
  = TBLTop
  | TBLBottom
  | TBLMiddle
```

##### Instances
``` purescript
EncodeJson TextBaseline
DecodeJson TextBaseline
```

#### `FontWeight`

``` purescript
data FontWeight
  = FWNormal
  | FWBold
  | FWBolder
  | FWLighter
  | FW100
  | FW200
  | FW300
  | FW400
  | FW500
  | FW600
  | FW700
  | FW800
  | FW900
```

##### Instances
``` purescript
EncodeJson FontWeight
DecodeJson FontWeight
```

#### `FontStyle`

``` purescript
data FontStyle
  = FSNormal
  | FSItalic
  | FSOblique
```

##### Instances
``` purescript
EncodeJson FontStyle
DecodeJson FontStyle
```

#### `FontFamily`

``` purescript
type FontFamily = String
```

#### `Decoration`

``` purescript
type Decoration = String
```

#### `textStyleDefault`

``` purescript
textStyleDefault :: TextStyleRec
```

### Re-exported from ECharts.Symbol:

#### `SymbolSize`

``` purescript
data SymbolSize
  = Size Number
  | Func (ItemValue -> Number)
  | ArrayMappingFunc (Array Number -> Number)
```

##### Instances
``` purescript
EncodeJson SymbolSize
DecodeJson SymbolSize
```

#### `Symbol`

``` purescript
data Symbol
  = Circle
  | Rectangle
  | Triangle
  | Diamond
  | EmptyCircle
  | EmptyRectangle
  | EmptyTriangle
  | EmptyDiamond
  | NoSymbol
```

##### Instances
``` purescript
EncodeJson Symbol
DecodeJson Symbol
```

#### `DoubleSymbolSize`

``` purescript
data DoubleSymbolSize
  = DblSize (Tuple Number Number)
  | DblFunc (ItemValue -> Tuple Number Number)
```

##### Instances
``` purescript
EncodeJson DoubleSymbolSize
DecodeJson DoubleSymbolSize
```

### Re-exported from ECharts.Timeline:

#### `TimelineType`

``` purescript
data TimelineType
  = TimelineTime
  | TimelineNumber
```

##### Instances
``` purescript
EncodeJson TimelineType
DecodeJson TimelineType
```

#### `TimelineRec`

``` purescript
type TimelineRec = { show :: Maybe Boolean, type :: Maybe TimelineType, notMerge :: Maybe Boolean, realtime :: Maybe Boolean, x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, padding :: Maybe (Corner Number), controlPosition :: Maybe TimelineControlPosition, autoPlay :: Maybe Boolean, loop :: Maybe Boolean, playInterval :: Maybe Number, lineStyle :: Maybe LineStyle, label :: Maybe AxisLabel, checkpointStyle :: Maybe CheckpointStyle, controlStyle :: Maybe ItemStyle, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, currentIndex :: Maybe Number, data :: Maybe (Array String) }
```

#### `TimelineControlPosition`

``` purescript
data TimelineControlPosition
  = TCPLeft
  | TCPRight
  | TCPNone
```

##### Instances
``` purescript
EncodeJson TimelineControlPosition
DecodeJson TimelineControlPosition
```

#### `Timeline`

``` purescript
newtype Timeline
  = Timeline TimelineRec
```

##### Instances
``` purescript
EncodeJson Timeline
DecodeJson Timeline
```

#### `timelineDefault`

``` purescript
timelineDefault :: TimelineRec
```

### Re-exported from ECharts.Title:

#### `TitleRec`

``` purescript
type TitleRec = { text :: Maybe String, link :: Maybe String, target :: Maybe LinkTarget, subtext :: Maybe String, sublink :: Maybe String, subtarget :: Maybe LinkTarget, x :: Maybe XPos, y :: Maybe YPos, textAlign :: Maybe HorizontalAlign, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, textStyle :: Maybe TextStyle, subtextStyle :: Maybe TextStyle }
```

#### `Title`

``` purescript
newtype Title
  = Title TitleRec
```

##### Instances
``` purescript
EncodeJson Title
DecodeJson Title
```

#### `LinkTarget`

``` purescript
data LinkTarget
  = Self
  | Blank
```

##### Instances
``` purescript
EncodeJson LinkTarget
DecodeJson LinkTarget
```

#### `titleDefault`

``` purescript
titleDefault :: TitleRec
```

### Re-exported from ECharts.Toolbox:

#### `ToolboxRec`

``` purescript
type ToolboxRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemSize :: Maybe Number, color :: Maybe (Array Color), disableColor :: Maybe Color, effectiveColor :: Maybe Color, showTitle :: Maybe Boolean, textStyle :: Maybe TextStyle, feature :: Maybe Feature }
```

#### `Toolbox`

``` purescript
newtype Toolbox
  = Toolbox ToolboxRec
```

##### Instances
``` purescript
EncodeJson Toolbox
DecodeJson Toolbox
```

#### `SaveAsImageFeatureRec`

``` purescript
type SaveAsImageFeatureRec = { show :: Maybe Boolean, title :: Maybe String, type :: Maybe ImgType, lang :: Maybe (Array String) }
```

#### `SaveAsImageFeature`

``` purescript
newtype SaveAsImageFeature
  = SaveAsImageFeature SaveAsImageFeatureRec
```

##### Instances
``` purescript
EncodeJson SaveAsImageFeature
DecodeJson SaveAsImageFeature
```

#### `RestoreFeatureRec`

``` purescript
type RestoreFeatureRec = { show :: Maybe Boolean, title :: Maybe String }
```

#### `RestoreFeature`

``` purescript
newtype RestoreFeature
  = RestoreFeature RestoreFeatureRec
```

##### Instances
``` purescript
EncodeJson RestoreFeature
DecodeJson RestoreFeature
```

#### `MarkFeatureTitleRec`

``` purescript
type MarkFeatureTitleRec = { mark :: Maybe String, markUndo :: String, markClear :: String }
```

#### `MarkFeatureTitle`

``` purescript
newtype MarkFeatureTitle
  = MarkFeatureTitle MarkFeatureTitleRec
```

##### Instances
``` purescript
EncodeJson MarkFeatureTitle
DecodeJson MarkFeatureTitle
```

#### `MarkFeatureRec`

``` purescript
type MarkFeatureRec = { show :: Maybe Boolean, title :: Maybe MarkFeatureTitle, lineStyle :: Maybe LineStyle }
```

#### `MarkFeature`

``` purescript
newtype MarkFeature
  = MarkFeature MarkFeatureRec
```

##### Instances
``` purescript
EncodeJson MarkFeature
DecodeJson MarkFeature
```

#### `MagicTypeFeatureRec`

``` purescript
type MagicTypeFeatureRec = { show :: Maybe Boolean, title :: Maybe (StrMap String), option :: Maybe Json, type :: Maybe (Array MagicType) }
```

#### `MagicTypeFeature`

``` purescript
newtype MagicTypeFeature
  = MagicTypeFeature MagicTypeFeatureRec
```

##### Instances
``` purescript
EncodeJson MagicTypeFeature
DecodeJson MagicTypeFeature
```

#### `MagicType`

``` purescript
data MagicType
  = MagicLine
  | MagicBar
  | MagicStack
  | MagicTiled
  | MagicForce
  | MagicChord
  | MagicPie
  | MagicFunnel
```

##### Instances
``` purescript
EncodeJson MagicType
DecodeJson MagicType
```

#### `FeatureRec`

``` purescript
type FeatureRec = { mark :: Maybe MarkFeature, dataZoom :: Maybe DataZoomFeature, dataView :: Maybe DataViewFeature, magicType :: Maybe MagicTypeFeature, restore :: Maybe RestoreFeature, saveAsImage :: Maybe SaveAsImageFeature }
```

#### `Feature`

``` purescript
newtype Feature
  = Feature FeatureRec
```

##### Instances
``` purescript
EncodeJson Feature
DecodeJson Feature
```

#### `DataZoomFeatureTitleRec`

``` purescript
type DataZoomFeatureTitleRec = { dataZoom :: String, dataZoomReset :: String }
```

#### `DataZoomFeatureTitle`

``` purescript
newtype DataZoomFeatureTitle
  = DataZoomFeatureTitle DataZoomFeatureTitleRec
```

##### Instances
``` purescript
EncodeJson DataZoomFeatureTitle
DecodeJson DataZoomFeatureTitle
```

#### `DataZoomFeatureRec`

``` purescript
type DataZoomFeatureRec = { show :: Maybe Boolean, title :: Maybe DataZoomFeatureTitle }
```

#### `DataZoomFeature`

``` purescript
newtype DataZoomFeature
  = DataZoomFeature DataZoomFeatureRec
```

##### Instances
``` purescript
EncodeJson DataZoomFeature
DecodeJson DataZoomFeature
```

#### `DataViewFeatureRec`

``` purescript
type DataViewFeatureRec = { show :: Maybe Boolean, title :: Maybe String, readOnly :: Maybe Boolean, lang :: Maybe (Array String) }
```

#### `DataViewFeature`

``` purescript
newtype DataViewFeature
  = DataViewFeature DataViewFeatureRec
```

##### Instances
``` purescript
EncodeJson DataViewFeature
DecodeJson DataViewFeature
```

#### `toolboxDefault`

``` purescript
toolboxDefault :: ToolboxRec
```

#### `saveAsImageFeatureDefault`

``` purescript
saveAsImageFeatureDefault :: SaveAsImageFeatureRec
```

#### `restoreFeatureDefault`

``` purescript
restoreFeatureDefault :: RestoreFeatureRec
```

#### `markFeatureDefault`

``` purescript
markFeatureDefault :: MarkFeatureRec
```

#### `magicTypeFeatureDefault`

``` purescript
magicTypeFeatureDefault :: MagicTypeFeatureRec
```

#### `featureDefault`

``` purescript
featureDefault :: FeatureRec
```

#### `dataZoomFeatureDefault`

``` purescript
dataZoomFeatureDefault :: DataZoomFeatureRec
```

#### `dataViewFeatureDefault`

``` purescript
dataViewFeatureDefault :: DataViewFeatureRec
```

### Re-exported from ECharts.Tooltip:

#### `TooltipTrigger`

``` purescript
data TooltipTrigger
  = TriggerItem
  | TriggerAxis
```

##### Instances
``` purescript
EncodeJson TooltipTrigger
DecodeJson TooltipTrigger
```

#### `TooltipRec`

``` purescript
type TooltipRec = { show :: Maybe Boolean, showContent :: Maybe Boolean, trigger :: Maybe TooltipTrigger, position :: Maybe TooltipPosition, formatter :: Maybe Formatter, islandFormatter :: Maybe Formatter, showDelay :: Maybe Number, hideDelay :: Maybe Number, transitionDuration :: Maybe Number, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderRadius :: Maybe Number, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), axisPointer :: Maybe TooltipAxisPointer, textStyle :: Maybe TextStyle, enterable :: Maybe Boolean }
```

#### `TooltipPosition`

``` purescript
data TooltipPosition
  = Fixed (Array Number)
  | FuncPos (Array Number -> Array Number)
```

##### Instances
``` purescript
EncodeJson TooltipPosition
DecodeJson TooltipPosition
```

#### `TooltipAxisPointerType`

``` purescript
data TooltipAxisPointerType
  = LinePointer
  | CrossPointer
  | ShadowPointer
  | NonePointer
```

##### Instances
``` purescript
EncodeJson TooltipAxisPointerType
DecodeJson TooltipAxisPointerType
```

#### `TooltipAxisPointerRec`

``` purescript
type TooltipAxisPointerRec = { type :: Maybe TooltipAxisPointerType, lineStyle :: Maybe LineStyle, crossStyle :: Maybe LineStyle, shadowStyle :: Maybe AreaStyle }
```

#### `TooltipAxisPointer`

``` purescript
newtype TooltipAxisPointer
  = TooltipAxisPointer TooltipAxisPointerRec
```

##### Instances
``` purescript
EncodeJson TooltipAxisPointer
DecodeJson TooltipAxisPointer
```

#### `Tooltip`

``` purescript
newtype Tooltip
  = Tooltip TooltipRec
```

##### Instances
``` purescript
EncodeJson Tooltip
DecodeJson Tooltip
```

#### `tooltipDefault`

``` purescript
tooltipDefault :: TooltipRec
```

#### `tooltipAxisPointerDefault`

``` purescript
tooltipAxisPointerDefault :: TooltipAxisPointerRec
```

### Re-exported from ECharts.Utils:

#### `unnull`

``` purescript
unnull :: Json -> Json
```

