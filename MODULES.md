# Module Documentation

## Module ECharts.AddData

#### `AdditionalDataRec`

``` purescript
type AdditionalDataRec = { additionalData :: Maybe String, dataGrow :: Boolean, isHead :: Boolean, datum :: ItemData, idx :: Number }
```


#### `AdditionalData`

``` purescript
newtype AdditionalData
  = AdditionalData AdditionalDataRec
```


#### `additionalDataEncodeJson`

``` purescript
instance additionalDataEncodeJson :: EncodeJson AdditionalData
```


#### `addData`

``` purescript
addData :: forall e. AdditionalData -> EChart -> Eff (dataAdd :: ADD_DATA | e) EChart
```



## Module ECharts.Axis

#### `AxisLineStyleRec`

``` purescript
type AxisLineStyleRec = { width :: Maybe Number, color :: Maybe [Tuple Number Color] }
```


#### `AxisLineStyle`

``` purescript
newtype AxisLineStyle
  = AxisLineStyle AxisLineStyleRec
```


#### `axisLineStyleEncodeJson`

``` purescript
instance axisLineStyleEncodeJson :: EncodeJson AxisLineStyle
```


#### `axisLineStyleDecodeJson`

``` purescript
instance axisLineStyleDecodeJson :: DecodeJson AxisLineStyle
```


#### `axisLineStyleDefault`

``` purescript
axisLineStyleDefault :: AxisLineStyleRec
```


#### `AxisLineRec`

``` purescript
type AxisLineRec = { lineStyle :: Maybe AxisLineStyle, onZero :: Maybe Boolean, show :: Maybe Boolean }
```


#### `AxisLine`

``` purescript
newtype AxisLine
  = AxisLine AxisLineRec
```


#### `axisLineEncodeJson`

``` purescript
instance axisLineEncodeJson :: EncodeJson AxisLine
```


#### `axisLineDecodeJson`

``` purescript
instance axisLineDecodeJson :: DecodeJson AxisLine
```


#### `axisLineDefault`

``` purescript
axisLineDefault :: AxisLineRec
```


#### `AxisTickRec`

``` purescript
type AxisTickRec = { inside :: Maybe Boolean, onGap :: Maybe Boolean, interval :: Maybe Interval, lineStyle :: Maybe LineStyle, length :: Maybe Number, splitNumber :: Maybe Number, show :: Maybe Boolean }
```


#### `AxisTick`

``` purescript
newtype AxisTick
  = AxisTick AxisTickRec
```


#### `axisTickEncodeJson`

``` purescript
instance axisTickEncodeJson :: EncodeJson AxisTick
```


#### `axisTickDecodeJson`

``` purescript
instance axisTickDecodeJson :: DecodeJson AxisTick
```


#### `axisTickDefault`

``` purescript
axisTickDefault :: AxisTickRec
```


#### `AxisLabelRec`

``` purescript
type AxisLabelRec = { clickable :: Maybe Boolean, margin :: Maybe Number, rotate :: Maybe Number, textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, interval :: Maybe Interval, show :: Maybe Boolean }
```


#### `AxisLabel`

``` purescript
newtype AxisLabel
  = AxisLabel AxisLabelRec
```


#### `axisLabelEncodeJson`

``` purescript
instance axisLabelEncodeJson :: EncodeJson AxisLabel
```


#### `axisLabelDecodeJson`

``` purescript
instance axisLabelDecodeJson :: DecodeJson AxisLabel
```


#### `axisLabelDefault`

``` purescript
axisLabelDefault :: AxisLabelRec
```


#### `Axises`

``` purescript
data Axises
  = OneAxis Axis
  | TwoAxises Axis Axis
```


#### `axisesEncodeJson`

``` purescript
instance axisesEncodeJson :: EncodeJson Axises
```


#### `axisesDecodeJson`

``` purescript
instance axisesDecodeJson :: DecodeJson Axises
```


#### `AxisSplitLineRec`

``` purescript
type AxisSplitLineRec = { lineStyle :: Maybe LineStyle, onGap :: Maybe Boolean, show :: Maybe Boolean }
```


#### `AxisSplitLine`

``` purescript
newtype AxisSplitLine
  = AxisSplitLine AxisSplitLineRec
```


#### `axisSplitLineEncodeJson`

``` purescript
instance axisSplitLineEncodeJson :: EncodeJson AxisSplitLine
```


#### `axisSplitLineDecodeJson`

``` purescript
instance axisSplitLineDecodeJson :: DecodeJson AxisSplitLine
```


#### `axisSplitLineDefault`

``` purescript
axisSplitLineDefault :: AxisSplitLineRec
```


#### `AxisSplitAreaRec`

``` purescript
type AxisSplitAreaRec = { areaStyle :: Maybe AreaStyle, onGap :: Maybe Boolean, show :: Maybe Boolean }
```


#### `AxisSplitArea`

``` purescript
newtype AxisSplitArea
  = AxisSplitArea AxisSplitAreaRec
```


#### `axisSplitAreaEncodeJson`

``` purescript
instance axisSplitAreaEncodeJson :: EncodeJson AxisSplitArea
```


#### `axisSplitAreaDecodeJson`

``` purescript
instance axisSplitAreaDecodeJson :: DecodeJson AxisSplitArea
```


#### `axisSplitAreaDefault`

``` purescript
axisSplitAreaDefault :: AxisSplitAreaRec
```


#### `AxisType`

``` purescript
data AxisType
  = CategoryAxis 
  | ValueAxis 
  | TimeAxis 
```


#### `axisTypeEncodeJson`

``` purescript
instance axisTypeEncodeJson :: EncodeJson AxisType
```


#### `axisTypeDecodeJson`

``` purescript
instance axisTypeDecodeJson :: DecodeJson AxisType
```


#### `AxisPosition`

``` purescript
data AxisPosition
  = LeftAxis 
  | RightAxis 
  | TopAxis 
  | BottomAxis 
```


#### `axisPositionEncodeJson`

``` purescript
instance axisPositionEncodeJson :: EncodeJson AxisPosition
```


#### `axisPositionDecodeJson`

``` purescript
instance axisPositionDecodeJson :: DecodeJson AxisPosition
```


#### `AxisNameLocation`

``` purescript
data AxisNameLocation
  = Start 
  | End 
```


#### `axisNameLocationEncodeJson`

``` purescript
instance axisNameLocationEncodeJson :: EncodeJson AxisNameLocation
```


#### `axisNameLocationDecodeJson`

``` purescript
instance axisNameLocationDecodeJson :: DecodeJson AxisNameLocation
```


#### `CustomAxisDataRec`

``` purescript
type CustomAxisDataRec = { textStyle :: TextStyle, value :: String }
```


#### `AxisData`

``` purescript
data AxisData
  = CommonAxisData String
  | CustomAxisData CustomAxisDataRec
```


#### `axisDataEncodeJson`

``` purescript
instance axisDataEncodeJson :: EncodeJson AxisData
```


#### `axisDataDecodeJson`

``` purescript
instance axisDataDecodeJson :: DecodeJson AxisData
```


#### `AxisBoundaryGap`

``` purescript
data AxisBoundaryGap
  = CatBoundaryGap Boolean
  | ValueBoundaryGap Number Number
```


#### `axisBoundaryGapEncodeJson`

``` purescript
instance axisBoundaryGapEncodeJson :: EncodeJson AxisBoundaryGap
```


#### `axisBoundaryGapDecodeJson`

``` purescript
instance axisBoundaryGapDecodeJson :: DecodeJson AxisBoundaryGap
```


#### `AxisRec`

``` purescript
type AxisRec = { "data" :: Maybe [AxisData], splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisTick :: Maybe AxisTick, axisLine :: Maybe AxisLine, splitNumber :: Maybe Number, scale :: Maybe Boolean, max :: Maybe Number, min :: Maybe Number, boundaryGap :: Maybe AxisBoundaryGap, nameTextStyle :: Maybe TextStyle, nameLocation :: Maybe AxisNameLocation, name :: Maybe String, position :: Maybe AxisPosition, show :: Maybe Boolean, "type" :: Maybe AxisType }
```


#### `Axis`

``` purescript
newtype Axis
  = Axis AxisRec
```


#### `axisDefault`

``` purescript
axisDefault :: AxisRec
```


#### `axisEncJson`

``` purescript
instance axisEncJson :: EncodeJson Axis
```


#### `axisDecJson`

``` purescript
instance axisDecJson :: DecodeJson Axis
```


#### `PolarNameRec`

``` purescript
type PolarNameRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, show :: Maybe Boolean }
```


#### `PolarName`

``` purescript
newtype PolarName
  = PolarName PolarNameRec
```


#### `polarNameEncode`

``` purescript
instance polarNameEncode :: EncodeJson PolarName
```


#### `polarNameDecodeJson`

``` purescript
instance polarNameDecodeJson :: DecodeJson PolarName
```


#### `polarNameDefault`

``` purescript
polarNameDefault :: PolarNameRec
```


#### `PolarType`

``` purescript
data PolarType
  = PolarPolygon 
  | PolarCircle 
```


#### `polarTypeEncode`

``` purescript
instance polarTypeEncode :: EncodeJson PolarType
```


#### `polarTypeDecodeJson`

``` purescript
instance polarTypeDecodeJson :: DecodeJson PolarType
```


#### `IndicatorRec`

``` purescript
type IndicatorRec = { axisLabel :: Maybe AxisLabel, max :: Maybe Number, min :: Maybe Number, text :: Maybe String }
```


#### `Indicator`

``` purescript
newtype Indicator
  = Indicator IndicatorRec
```


#### `indicatorEncodeJson`

``` purescript
instance indicatorEncodeJson :: EncodeJson Indicator
```


#### `indicatorDecodeJson`

``` purescript
instance indicatorDecodeJson :: DecodeJson Indicator
```


#### `indicatorDefault`

``` purescript
indicatorDefault :: IndicatorRec
```


#### `PolarRec`

``` purescript
type PolarRec = { indicator :: Maybe [Indicator], "type" :: Maybe PolarType, splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisLine :: Maybe AxisLine, scale :: Maybe Boolean, boundaryGap :: Maybe (Tuple Number Number), name :: Maybe PolarName, splitNumber :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe PercentOrPixel, center :: Maybe (Tuple PercentOrPixel PercentOrPixel) }
```


#### `Polar`

``` purescript
newtype Polar
  = Polar PolarRec
```


#### `polarEncodeJson`

``` purescript
instance polarEncodeJson :: EncodeJson Polar
```


#### `polarDecodeJson`

``` purescript
instance polarDecodeJson :: DecodeJson Polar
```


#### `polarDefault`

``` purescript
polarDefault :: PolarRec
```



## Module ECharts.Chart

#### `EChart`

``` purescript
data EChart :: *
```


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


#### `themeEncodeJson`

``` purescript
instance themeEncodeJson :: EncodeJson Theme
```


#### `init`

``` purescript
init :: forall e. Maybe Theme -> HTMLElement -> Eff (echartInit :: ECHARTS_INIT, dom :: DOM | e) EChart
```


#### `setTheme`

``` purescript
setTheme :: forall e. Theme -> EChart -> Eff (echartTheme :: ECHARTS_THEME_SET, dom :: DOM | e) EChart
```


#### `getZRender`

``` purescript
getZRender :: forall e. EChart -> Eff e ZRender
```


#### `resize`

``` purescript
resize :: forall e. EChart -> Eff (echartResize :: ECHARTS_RESIZE, dom :: DOM | e) Unit
```


#### `refresh`

``` purescript
refresh :: forall e. EChart -> Eff (echartRefresh :: ECHARTS_REFRESH, dom :: DOM | e) Unit
```


#### `clear`

``` purescript
clear :: forall e. EChart -> Eff (echartClear :: ECHARTS_CLEAR, dom :: DOM | e) Unit
```


#### `dispose`

``` purescript
dispose :: forall e. EChart -> Eff (echartDispose :: ECHARTS_DISPOSE, dom :: DOM | e) Unit
```



## Module ECharts.Color

#### `Color`

``` purescript
type Color = String
```


#### `ColorFuncParamRec`

``` purescript
type ColorFuncParamRec = { "data" :: { name :: String, value :: ItemValue }, dataIndex :: Number, series :: String, seriesIndex :: Number }
```


#### `ColorFuncParam`

``` purescript
newtype ColorFuncParam
  = ColorFuncParam ColorFuncParamRec
```


#### `CalculableColor`

``` purescript
data CalculableColor
  = SimpleColor Color
  | ColorFunc (ColorFuncParam -> Color)
```


#### `calculableColorEncodeJson`

``` purescript
instance calculableColorEncodeJson :: EncodeJson CalculableColor
```


#### `calculableColorDecodeJson`

``` purescript
instance calculableColorDecodeJson :: DecodeJson CalculableColor
```



## Module ECharts.Common

#### `GeoCoord`

``` purescript
type GeoCoord = M.StrMap (Tuple Number Number)
```


#### `Corner`

``` purescript
data Corner a
  = AllCorners a
  | Corners a a a a
```


#### `cornerJsonEncode`

``` purescript
instance cornerJsonEncode :: (EncodeJson a) => EncodeJson (Corner a)
```


#### `cornerJsonDecode`

``` purescript
instance cornerJsonDecode :: (DecodeJson a) => DecodeJson (Corner a)
```


#### `PercentOrPixel`

``` purescript
data PercentOrPixel
  = Percent Number
  | Pixel Number
```


#### `percentOrPixelEncodeJson`

``` purescript
instance percentOrPixelEncodeJson :: EncodeJson PercentOrPixel
```


#### `percentOrPixelDecodeJson`

``` purescript
instance percentOrPixelDecodeJson :: DecodeJson PercentOrPixel
```


#### `RoseType`

``` purescript
data RoseType
  = RTRadius 
  | RTArea 
```


#### `roseTypeEncodeJson`

``` purescript
instance roseTypeEncodeJson :: EncodeJson RoseType
```


#### `roseTypeDecodeJson`

``` purescript
instance roseTypeDecodeJson :: DecodeJson RoseType
```


#### `SelectedMode`

``` purescript
data SelectedMode
  = SelModeSingle 
  | SelModeMultiple 
  | SelModeFalse 
```


#### `selModeEncodeJson`

``` purescript
instance selModeEncodeJson :: EncodeJson SelectedMode
```


#### `selModeDecodeJson`

``` purescript
instance selModeDecodeJson :: DecodeJson SelectedMode
```


#### `MapValueCalculation`

``` purescript
data MapValueCalculation
  = SumCalculation 
  | AverageCalculation 
```


#### `mapValueCalculationEncodeJson`

``` purescript
instance mapValueCalculationEncodeJson :: EncodeJson MapValueCalculation
```


#### `mapValueCalculationDecodeJson`

``` purescript
instance mapValueCalculationDecodeJson :: DecodeJson MapValueCalculation
```


#### `Roam`

``` purescript
data Roam
  = Enable 
  | Disable 
  | Scale 
  | Move 
```


#### `roamEncodeJson`

``` purescript
instance roamEncodeJson :: EncodeJson Roam
```


#### `roamDecodeJson`

``` purescript
instance roamDecodeJson :: DecodeJson Roam
```


#### `MinMaxRec`

``` purescript
type MinMaxRec = { max :: Number, min :: Number }
```


#### `MinMax`

``` purescript
newtype MinMax
  = MinMax MinMaxRec
```


#### `minMaxEncodeJson`

``` purescript
instance minMaxEncodeJson :: EncodeJson MinMax
```


#### `minMaxDecodeJson`

``` purescript
instance minMaxDecodeJson :: DecodeJson MinMax
```


#### `Center`

``` purescript
type Center = Tuple PercentOrPixel PercentOrPixel
```


#### `RsRec`

``` purescript
type RsRec = { outer :: PercentOrPixel, inner :: PercentOrPixel }
```


#### `Radius`

``` purescript
data Radius
  = R PercentOrPixel
  | Rs RsRec
```


#### `radiusEncodeJson`

``` purescript
instance radiusEncodeJson :: EncodeJson Radius
```


#### `radiusDecodeJson`

``` purescript
instance radiusDecodeJson :: DecodeJson Radius
```


#### `Sort`

``` purescript
data Sort
  = NoSort 
  | Asc 
  | Desc 
```


#### `sortEncodeJson`

``` purescript
instance sortEncodeJson :: EncodeJson Sort
```


#### `sortDecodeJson`

``` purescript
instance sortDecodeJson :: DecodeJson Sort
```


#### `Interval`

``` purescript
data Interval
  = Auto 
  | Custom Number
```


#### `intervalEncodeJson`

``` purescript
instance intervalEncodeJson :: EncodeJson Interval
```


#### `intervalDecodeJson`

``` purescript
instance intervalDecodeJson :: DecodeJson Interval
```



## Module ECharts.Connect

#### `Connection`

``` purescript
newtype Connection
```


#### `connect`

``` purescript
connect :: forall e. EChart -> EChart -> Eff (connect :: CONNECT | e) Connection
```



## Module ECharts.Coords

#### `XPos`

``` purescript
data XPos
  = XLeft 
  | XRight 
  | XCenter 
  | X Number
```


#### `xPosEncodeJson`

``` purescript
instance xPosEncodeJson :: EncodeJson XPos
```


#### `xPosDecodeJson`

``` purescript
instance xPosDecodeJson :: DecodeJson XPos
```


#### `YPos`

``` purescript
data YPos
  = YTop 
  | YBottom 
  | YCenter 
  | Y Number
```


#### `yPosEncodeJson`

``` purescript
instance yPosEncodeJson :: EncodeJson YPos
```


#### `yPosDecodeJson`

``` purescript
instance yPosDecodeJson :: DecodeJson YPos
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


#### `labelPositionEncodeJson`

``` purescript
instance labelPositionEncodeJson :: EncodeJson LabelPosition
```


#### `labelPositionDecodeJson`

``` purescript
instance labelPositionDecodeJson :: DecodeJson LabelPosition
```


#### `HorizontalAlign`

``` purescript
data HorizontalAlign
  = HAlignLeft 
  | HAlignRight 
  | HAlignCenter 
```


#### `textAlignEncodeJson`

``` purescript
instance textAlignEncodeJson :: EncodeJson HorizontalAlign
```


#### `textAlignDecodeJson`

``` purescript
instance textAlignDecodeJson :: DecodeJson HorizontalAlign
```


#### `LocationRec`

``` purescript
type LocationRec = { y :: Maybe YPos, x :: Maybe XPos }
```


#### `Location`

``` purescript
newtype Location
  = Location LocationRec
```


#### `locationEncodeJson`

``` purescript
instance locationEncodeJson :: EncodeJson Location
```


#### `locationDecodeJson`

``` purescript
instance locationDecodeJson :: DecodeJson Location
```


#### `Orient`

``` purescript
data Orient
  = Horizontal 
  | Vertical 
```


#### `orientEncodeJson`

``` purescript
instance orientEncodeJson :: EncodeJson Orient
```


#### `orientDecodeJson`

``` purescript
instance orientDecodeJson :: DecodeJson Orient
```



## Module ECharts.DataRange

#### `DataRangeRec`

``` purescript
type DataRangeRec = { textStyle :: Maybe TextStyle, text :: Maybe (Tuple String String), formatter :: Maybe Formatter, color :: Maybe [Color], realtime :: Maybe Boolean, hoverLink :: Maybe Boolean, calculable :: Maybe Boolean, selectedMode :: Maybe SelectedMode, splitNumber :: Maybe Number, precision :: Maybe Number, max :: Maybe Number, min :: Maybe Number, itemHeight :: Maybe Number, itemWidth :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }
```


#### `DataRange`

``` purescript
newtype DataRange
  = DataRange DataRangeRec
```


#### `dataRangeEncodeJson`

``` purescript
instance dataRangeEncodeJson :: EncodeJson DataRange
```


#### `dataRangeDecodeJson`

``` purescript
instance dataRangeDecodeJson :: DecodeJson DataRange
```


#### `dataRangeDefault`

``` purescript
dataRangeDefault :: DataRangeRec
```



## Module ECharts.DataZoom

#### `DataZoomRec`

``` purescript
type DataZoomRec = { zoomlock :: Maybe Boolean, realtime :: Maybe Boolean, showDetail :: Maybe Boolean, end :: Maybe Number, start :: Maybe Number, yAxisIndex :: Maybe [Number], xAxisIndex :: Maybe [Number], handleColor :: Maybe Color, fillerColor :: Maybe Color, dataBackgroundColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }
```


#### `DataZoom`

``` purescript
newtype DataZoom
  = DataZoom DataZoomRec
```


#### `dataZoomEncodeJson`

``` purescript
instance dataZoomEncodeJson :: EncodeJson DataZoom
```


#### `dataZoomDecodeJson`

``` purescript
instance dataZoomDecodeJson :: DecodeJson DataZoom
```


#### `dataZoomDefault`

``` purescript
dataZoomDefault :: DataZoomRec
```



## Module ECharts.Effects

#### `ECHARTS_INIT`

``` purescript
data ECHARTS_INIT :: !
```


#### `ECHARTS_RESIZE`

``` purescript
data ECHARTS_RESIZE :: !
```


#### `ECHARTS_CLEAR`

``` purescript
data ECHARTS_CLEAR :: !
```


#### `ECHARTS_REFRESH`

``` purescript
data ECHARTS_REFRESH :: !
```


#### `ECHARTS_DISPOSE`

``` purescript
data ECHARTS_DISPOSE :: !
```


#### `ECHARTS_THEME_SET`

``` purescript
data ECHARTS_THEME_SET :: !
```


#### `ECHARTS_OPTION_SET`

``` purescript
data ECHARTS_OPTION_SET :: !
```


#### `ADD_DATA`

``` purescript
data ADD_DATA :: !
```


#### `CONNECT`

``` purescript
data CONNECT :: !
```


#### `DISCONNECT`

``` purescript
data DISCONNECT :: !
```


#### `LISTEN`

``` purescript
data LISTEN :: !
```


#### `UNLISTEN`

``` purescript
data UNLISTEN :: !
```


#### `IMAGE_MAKING`

``` purescript
data IMAGE_MAKING :: !
```


#### `LOADING_SHOW`

``` purescript
data LOADING_SHOW :: !
```


#### `LOADING_HIDE`

``` purescript
data LOADING_HIDE :: !
```


#### `ADD_MARKLINE`

``` purescript
data ADD_MARKLINE :: !
```


#### `REMOVE_MARKLINE`

``` purescript
data REMOVE_MARKLINE :: !
```


#### `ADD_MARKPOINT`

``` purescript
data ADD_MARKPOINT :: !
```


#### `REMOVE_MARKPOINT`

``` purescript
data REMOVE_MARKPOINT :: !
```



## Module ECharts.Events

#### `EventType`

``` purescript
data EventType
  = Refresh 
  | Restore 
  | Resize 
  | Click 
  | DoubleClick 
  | Hover 
  | DataChanged 
  | DataZoom 
  | DataRange 
  | DataRangeHoverLink 
  | LegendSelected 
  | LegendHoverLink 
  | MapSelected 
  | PieSelected 
  | DataViewChanged 
  | MapRoam 
  | MagicTypeChanged 
```


#### `EventParam`

``` purescript
type EventParam = Json
```


#### `Sub`

``` purescript
newtype Sub
```


#### `listen`

``` purescript
listen :: forall e. EventType -> (EventParam -> Eff (listen :: LISTEN | e) Unit) -> EChart -> Eff (listen :: LISTEN | e) Sub
```



## Module ECharts.Formatter

#### `FormatParams`

``` purescript
type FormatParams = Json
```


#### `Formatter`

``` purescript
data Formatter
  = Template String
  | FormatFunc (forall eff. [FormatParams] -> Eff eff String)
```


#### `formatterEncodeJson`

``` purescript
instance formatterEncodeJson :: EncodeJson Formatter
```


#### `formatterDecodeJson`

``` purescript
instance formatterDecodeJson :: DecodeJson Formatter
```



## Module ECharts.Grid

#### `GridRec`

``` purescript
type GridRec = { borderColor :: Maybe Number, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel }
```


#### `Grid`

``` purescript
newtype Grid
  = Grid GridRec
```


#### `gridEncodeJson`

``` purescript
instance gridEncodeJson :: EncodeJson Grid
```


#### `gridDecodeJson`

``` purescript
instance gridDecodeJson :: DecodeJson Grid
```


#### `gridDefault`

``` purescript
gridDefault :: GridRec
```



## Module ECharts.Image

#### `ImgType`

``` purescript
data ImgType
  = PNG 
  | JPEG 
```


#### `encodeImg`

``` purescript
instance encodeImg :: EncodeJson ImgType
```


#### `decodeImg`

``` purescript
instance decodeImg :: DecodeJson ImgType
```


#### `getDataURL`

``` purescript
getDataURL :: forall e. ImgType -> EChart -> Eff (image :: IMAGE_MAKING | e) String
```


#### `getImage`

``` purescript
getImage :: forall e. ImgType -> EChart -> Eff (image :: IMAGE_MAKING, dom :: DOM | e) Node
```



## Module ECharts.Legend

#### `LegendItemRec`

``` purescript
type LegendItemRec = { textStyle :: Maybe TextStyle, icon :: Maybe String }
```


#### `LegendItem`

``` purescript
data LegendItem
  = LegendItem String LegendItemRec
```


#### `legendItemEncodeJson`

``` purescript
instance legendItemEncodeJson :: EncodeJson LegendItem
```


#### `legendItemDecodeJson`

``` purescript
instance legendItemDecodeJson :: DecodeJson LegendItem
```


#### `legendItemDefault`

``` purescript
legendItemDefault :: String -> LegendItem
```


#### `LegendRec`

``` purescript
type LegendRec = { "data" :: Maybe [LegendItem], selected :: Maybe (StrMap Boolean), selectedMode :: Maybe SelectedMode, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle, itemWidth :: Maybe Number, itemHeight :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }
```


#### `Legend`

``` purescript
newtype Legend
  = Legend LegendRec
```


#### `legendDefault`

``` purescript
legendDefault :: LegendRec
```


#### `legendEncodeJson`

``` purescript
instance legendEncodeJson :: EncodeJson Legend
```


#### `legendDecodeJson`

``` purescript
instance legendDecodeJson :: DecodeJson Legend
```



## Module ECharts.Loading

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


#### `loadingEffectEncodeJson`

``` purescript
instance loadingEffectEncodeJson :: EncodeJson LoadingEffect
```


#### `LoadingOptionRec`

``` purescript
type LoadingOptionRec = { progress :: Maybe Number, effectOption :: Maybe Json, effect :: Maybe LoadingEffect, textStyle :: Maybe TextStyle, y :: Maybe YPos, x :: Maybe XPos, text :: Maybe String }
```


#### `LoadingOption`

``` purescript
newtype LoadingOption
  = LoadingOption LoadingOptionRec
```


#### `showLoadingOptions`

``` purescript
instance showLoadingOptions :: EncodeJson LoadingOption
```


#### `showLoading`

``` purescript
showLoading :: forall e. LoadingOption -> EChart -> Eff (showLoadingECharts :: LOADING_SHOW | e) EChart
```


#### `hideLoading`

``` purescript
hideLoading :: forall e. EChart -> Eff (hideLoadingECharts :: LOADING_HIDE | e) EChart
```


#### `loadingOptionDefault`

``` purescript
loadingOptionDefault :: LoadingOptionRec
```



## Module ECharts.Options

#### `OptionRec`

``` purescript
type OptionRec = { series :: Maybe [Maybe Series], polar :: Maybe [Polar], yAxis :: Maybe Axises, xAxis :: Maybe Axises, grid :: Maybe Grid, roamController :: Maybe RoamController, dataZoom :: Maybe DataZoom, dataRange :: Maybe DataRange, legend :: Maybe Legend, title :: Maybe Title, toolbox :: Maybe Toolbox, tooltip :: Maybe Tooltip, timeline :: Maybe Timeline, animation :: Maybe Boolean, calculable :: Maybe Boolean, renderAsImage :: Maybe Boolean, color :: Maybe [Color], backgroundColor :: Maybe Color }
```

#### `Option`

``` purescript
newtype Option
  = Option OptionRec
```


#### `optionsEncodeJson`

``` purescript
instance optionsEncodeJson :: EncodeJson Option
```


#### `optionsDecodeJson`

``` purescript
instance optionsDecodeJson :: DecodeJson Option
```


#### `optionDefault`

``` purescript
optionDefault :: OptionRec
```


#### `setOption`

``` purescript
setOption :: forall e. Option -> Boolean -> EChart -> Eff (echartSetOption :: ECHARTS_OPTION_SET | e) EChart
```



## Module ECharts.RoamController

#### `RoamControllerRec`

``` purescript
type RoamControllerRec = { mapTypeControl :: Maybe (StrMap Boolean), step :: Maybe Number, handleColor :: Maybe Color, fillerColor :: Maybe Color, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, show :: Maybe Boolean }
```


#### `RoamController`

``` purescript
newtype RoamController
  = RoamController RoamControllerRec
```


#### `roamControllerEncodeJson`

``` purescript
instance roamControllerEncodeJson :: EncodeJson RoamController
```


#### `roamControllerDecodeJson`

``` purescript
instance roamControllerDecodeJson :: DecodeJson RoamController
```


#### `roamControllerDefault`

``` purescript
roamControllerDefault :: RoamControllerRec
```



## Module ECharts.Series

#### `chartTypeEncodeJson`

``` purescript
instance chartTypeEncodeJson :: EncodeJson ChartType
```


#### `Series`

``` purescript
data Series
  = LineSeries { lineSeries :: LineSeriesRec, common :: UniversalSeriesRec }
  | BarSeries { barSeries :: BarSeriesRec, common :: UniversalSeriesRec }
  | ScatterSeries { scatterSeries :: ScatterSeriesRec, common :: UniversalSeriesRec }
  | CandlestickSeries { candlestickSeries :: CandlestickSeriesRec, common :: UniversalSeriesRec }
  | PieSeries { pieSeries :: PieSeriesRec, common :: UniversalSeriesRec }
  | RadarSeries { radarSeries :: RadarSeriesRec, common :: UniversalSeriesRec }
  | ChordSeries { chordSeries :: ChordSeriesRec, common :: UniversalSeriesRec }
  | ForceSeries { forceSeries :: ForceSeriesRec, common :: UniversalSeriesRec }
  | MapSeries { mapSeries :: MapSeriesRec, common :: UniversalSeriesRec }
  | GaugeSeries { gaugeSeries :: GaugeSeriesRec, common :: UniversalSeriesRec }
  | FunnelSeries { funnelSeries :: FunnelSeriesRec, common :: UniversalSeriesRec }
  | EventRiverSeries { eventRiverSeries :: EventRiverSeriesRec, common :: UniversalSeriesRec }
```


#### `UniversalSeriesRec`

``` purescript
type UniversalSeriesRec = { markLine :: Maybe MarkLine, markPoint :: Maybe MarkPoint, itemStyle :: Maybe ItemStyle, clickable :: Maybe Boolean, tooltip :: Maybe Tooltip, name :: Maybe String }
```


#### `universalSeriesDefault`

``` purescript
universalSeriesDefault :: UniversalSeriesRec
```


#### `LineSeriesRec`

``` purescript
type LineSeriesRec = { legendHoverLink :: Maybe Boolean, smooth :: Maybe Boolean, showAllSymbol :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, stack :: Maybe String, "data" :: Maybe [ItemData] }
```


#### `lineSeriesDefault`

``` purescript
lineSeriesDefault :: LineSeriesRec
```


#### `BarSeriesRec`

``` purescript
type BarSeriesRec = { legendHoverLink :: Maybe Boolean, barMaxWidth :: Maybe Number, barWidth :: Maybe Number, barMinHeight :: Maybe Number, barCategoryGap :: Maybe PercentOrPixel, barGap :: Maybe PercentOrPixel, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, stack :: Maybe String, "data" :: Maybe [ItemData] }
```


#### `barSeriesDefault`

``` purescript
barSeriesDefault :: BarSeriesRec
```


#### `ScatterSeriesRec`

``` purescript
type ScatterSeriesRec = { legendHoverLink :: Maybe Boolean, largeThreshold :: Maybe Number, large :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, "data" :: Maybe [ItemData] }
```


#### `scatterSeriesDefault`

``` purescript
scatterSeriesDefault :: ScatterSeriesRec
```


#### `CandlestickSeriesRec`

``` purescript
type CandlestickSeriesRec = { barMaxWidth :: Maybe Number, barWidth :: Maybe Number, barMinHeight :: Maybe Number, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, "data" :: Maybe [ItemData] }
```


#### `candlestickSeriesDefault`

``` purescript
candlestickSeriesDefault :: CandlestickSeriesRec
```


#### `PieSeriesRec`

``` purescript
type PieSeriesRec = { legendHoverLink :: Maybe Boolean, selectedMode :: Maybe SelectedMode, selectedOffset :: Maybe Number, roseType :: Maybe RoseType, clockWise :: Maybe Boolean, minAngle :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe Radius, center :: Maybe Center, "data" :: Maybe [ItemData] }
```


#### `pieSeriesDefault`

``` purescript
pieSeriesDefault :: PieSeriesRec
```


#### `RadarSeriesRec`

``` purescript
type RadarSeriesRec = { legendHoverLink :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, polarIndex :: Maybe Number, "data" :: Maybe [ItemData] }
```


#### `radarSeriesDefault`

``` purescript
radarSeriesDefault :: RadarSeriesRec
```


#### `ChordSeriesRec`

``` purescript
type ChordSeriesRec = { clockWise :: Maybe Boolean, sortSub :: Maybe Sort, sort :: Maybe Sort, padding :: Maybe Number, showScaleText :: Maybe Boolean, showScale :: Maybe Boolean, maxRadius :: Maybe Number, minRadius :: Maybe Number, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, ribbonType :: Maybe Boolean, "data" :: Maybe [ItemData], matrix :: Maybe Matrix, links :: Maybe [Link], categories :: Maybe [ForceCategory], nodes :: Maybe [Node] }
```


#### `chordSeriesDefault`

``` purescript
chordSeriesDefault :: ChordSeriesRec
```


#### `ForceSeriesRec`

``` purescript
type ForceSeriesRec = { ribbonType :: Maybe Boolean, steps :: Maybe Number, useWorker :: Maybe Boolean, large :: Maybe Boolean, draggable :: Maybe Number, gravity :: Maybe Number, scaling :: Maybe Number, linkSymbolSize :: Maybe Symbol, linkSymbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, maxRadius :: Maybe Number, minRadius :: Maybe Number, size :: Maybe Number, center :: Maybe Center, matrix :: Maybe Matrix, links :: Maybe [Link], nodes :: Maybe [Node], categories :: Maybe [ForceCategory] }
```


#### `forceSeriesDefault`

``` purescript
forceSeriesDefault :: ForceSeriesRec
```


#### `MapSeriesRec`

``` purescript
type MapSeriesRec = { geoCoord :: Maybe (StrMap (Tuple Number Number)), textFixed :: Maybe (StrMap (Tuple Number Number)), nameMap :: Maybe (StrMap String), scaleLimit :: Maybe MinMax, roam :: Maybe Roam, showLegendSymbol :: Maybe Boolean, mapValuePrecision :: Maybe Number, mapValueCalculation :: Maybe MapValueCalculation, mapLocation :: Maybe Location, dataRangeHoverLink :: Maybe Boolean, hoverable :: Maybe Boolean, mapType :: Maybe String, selectedMode :: Maybe SelectedMode, "data" :: Maybe [ItemData] }
```


#### `mapSeriesDefault`

``` purescript
mapSeriesDefault :: MapSeriesRec
```


#### `GaugeSeriesRec`

``` purescript
type GaugeSeriesRec = { legendHoverLink :: Maybe Boolean, pointer :: Maybe Pointer, detail :: Maybe GaugeDetail, title :: Maybe Title, splitLine :: Maybe SplitLine, axisLabel :: Maybe AxisLabel, axisTick :: Maybe AxisTick, axisLine :: Maybe AxisLine, splitNumber :: Maybe Number, precision :: Maybe Number, max :: Maybe Number, min :: Maybe Number, endAngle :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe Radius, center :: Maybe (Tuple Number Number), "data" :: Maybe [ItemData] }
```


#### `gaugeSeriesDefault`

``` purescript
gaugeSeriesDefault :: GaugeSeriesRec
```


#### `FunnelSeriesRec`

``` purescript
type FunnelSeriesRec = { legendHoverLink :: Maybe Boolean, sort :: Maybe Sort, gap :: Maybe Number, maxSize :: Maybe PercentOrPixel, minSize :: Maybe PercentOrPixel, max :: Maybe Number, min :: Maybe Number, funnelAlign :: Maybe HorizontalAlign, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel, "data" :: Maybe [ItemData] }
```


#### `funnelSeriesDefault`

``` purescript
funnelSeriesDefault :: FunnelSeriesRec
```


#### `EventRiverSeriesRec`

``` purescript
type EventRiverSeriesRec = { legendHoverLink :: Maybe Boolean, weight :: Maybe Number, xAxisIndex :: Maybe Number, eventList :: Maybe [OneEvent] }
```


#### `eventRiverSeriesDefault`

``` purescript
eventRiverSeriesDefault :: EventRiverSeriesRec
```


#### `encodeSeries`

``` purescript
instance encodeSeries :: EncodeJson Series
```


#### `decodeSeries`

``` purescript
instance decodeSeries :: DecodeJson Series
```


#### `setSeries`

``` purescript
setSeries :: forall e. [Series] -> Boolean -> EChart -> Eff e EChart
```



## Module ECharts.Symbol

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
```


#### `encodeJsonSymbol`

``` purescript
instance encodeJsonSymbol :: EncodeJson Symbol
```


#### `symbolDecodeJson`

``` purescript
instance symbolDecodeJson :: DecodeJson Symbol
```


#### `SymbolSize`

``` purescript
data SymbolSize
  = Size Number
  | Func (ItemValue -> Number)
```


#### `symbolSizeEncodeJson`

``` purescript
instance symbolSizeEncodeJson :: EncodeJson SymbolSize
```


#### `symbolSizeDecodeJson`

``` purescript
instance symbolSizeDecodeJson :: DecodeJson SymbolSize
```


#### `DoubleSymbolSize`

``` purescript
data DoubleSymbolSize
  = DblSize (Tuple Number Number)
  | DblFunc (ItemValue -> Tuple Number Number)
```


#### `dblSymbolSizeEncodeJson`

``` purescript
instance dblSymbolSizeEncodeJson :: EncodeJson DoubleSymbolSize
```


#### `dblSymbolSizeDecodeJson`

``` purescript
instance dblSymbolSizeDecodeJson :: DecodeJson DoubleSymbolSize
```



## Module ECharts.Timeline

#### `TimelineType`

``` purescript
data TimelineType
  = TimelineTime 
  | TimelineNumber 
```


#### `timelineTypeEncodeJson`

``` purescript
instance timelineTypeEncodeJson :: EncodeJson TimelineType
```


#### `timelineTypeDecodeJson`

``` purescript
instance timelineTypeDecodeJson :: DecodeJson TimelineType
```


#### `TimelineControlPosition`

``` purescript
data TimelineControlPosition
  = TCPLeft 
  | TCPRight 
  | TCPNone 
```


#### `timelineControlPositionEncodeJson`

``` purescript
instance timelineControlPositionEncodeJson :: EncodeJson TimelineControlPosition
```


#### `timelineControlPositionDecodeJson`

``` purescript
instance timelineControlPositionDecodeJson :: DecodeJson TimelineControlPosition
```


#### `TimelineRec`

``` purescript
type TimelineRec = { "data" :: Maybe [String], currentIndex :: Maybe Number, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, controlStyle :: Maybe ItemStyle, checkpointStyle :: Maybe CheckpointStyle, label :: Maybe AxisLabel, lineStyle :: Maybe LineStyle, playInterval :: Maybe Number, loop :: Maybe Boolean, autoPlay :: Maybe Boolean, controlPosition :: Maybe TimelineControlPosition, padding :: Maybe (Corner Number), borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel, realtime :: Maybe Boolean, notMerge :: Maybe Boolean, "type" :: Maybe TimelineType, show :: Maybe Boolean }
```


#### `Timeline`

``` purescript
newtype Timeline
  = Timeline TimelineRec
```


#### `timelineEncodeJson`

``` purescript
instance timelineEncodeJson :: EncodeJson Timeline
```


#### `timelineDecodeJson`

``` purescript
instance timelineDecodeJson :: DecodeJson Timeline
```


#### `timelineDefault`

``` purescript
timelineDefault :: TimelineRec
```



## Module ECharts.Title

#### `LinkTarget`

``` purescript
data LinkTarget
  = Self 
  | Blank 
```


#### `linkTargetEncodeJson`

``` purescript
instance linkTargetEncodeJson :: EncodeJson LinkTarget
```


#### `linkTargetDecodeJson`

``` purescript
instance linkTargetDecodeJson :: DecodeJson LinkTarget
```


#### `TitleRec`

``` purescript
type TitleRec = { subtextStyle :: Maybe TextStyle, textStyle :: Maybe TextStyle, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, textAlign :: Maybe HorizontalAlign, y :: Maybe YPos, x :: Maybe XPos, subtarget :: Maybe LinkTarget, sublink :: Maybe String, subtext :: Maybe String, target :: Maybe LinkTarget, link :: Maybe String, text :: Maybe String }
```


#### `Title`

``` purescript
newtype Title
  = Title TitleRec
```


#### `titleEncodeJson`

``` purescript
instance titleEncodeJson :: EncodeJson Title
```


#### `titleDecodeJson`

``` purescript
instance titleDecodeJson :: DecodeJson Title
```


#### `titleDefault`

``` purescript
titleDefault :: TitleRec
```



## Module ECharts.Toolbox

#### `ToolboxRec`

``` purescript
type ToolboxRec = { feature :: Maybe Feature, textStyle :: Maybe TextStyle, showTitle :: Maybe Boolean, effectiveColor :: Maybe Color, disableColor :: Maybe Color, color :: Maybe [Color], itemSize :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }
```


#### `Toolbox`

``` purescript
newtype Toolbox
  = Toolbox ToolboxRec
```


#### `toolboxDefault`

``` purescript
toolboxDefault :: ToolboxRec
```


#### `toolboxEncodeJson`

``` purescript
instance toolboxEncodeJson :: EncodeJson Toolbox
```


#### `toolboxDecodeJson`

``` purescript
instance toolboxDecodeJson :: DecodeJson Toolbox
```


#### `FeatureRec`

``` purescript
type FeatureRec = { saveAsImage :: Maybe SaveAsImageFeature, restore :: Maybe RestoreFeature, magicType :: Maybe MagicTypeFeature, dataView :: Maybe DataViewFeature, dataZoom :: Maybe DataZoomFeature, mark :: Maybe MarkFeature }
```


#### `Feature`

``` purescript
newtype Feature
  = Feature FeatureRec
```


#### `featureEncodeJson`

``` purescript
instance featureEncodeJson :: EncodeJson Feature
```


#### `featureDecodeJson`

``` purescript
instance featureDecodeJson :: DecodeJson Feature
```


#### `featureDefault`

``` purescript
featureDefault :: FeatureRec
```


#### `SaveAsImageFeatureRec`

``` purescript
type SaveAsImageFeatureRec = { lang :: Maybe [String], "type" :: Maybe ImgType, title :: Maybe String, show :: Maybe Boolean }
```


#### `SaveAsImageFeature`

``` purescript
newtype SaveAsImageFeature
  = SaveAsImageFeature SaveAsImageFeatureRec
```


#### `saveAsImageEncodeJson`

``` purescript
instance saveAsImageEncodeJson :: EncodeJson SaveAsImageFeature
```


#### `saveAsImageDecodeJson`

``` purescript
instance saveAsImageDecodeJson :: DecodeJson SaveAsImageFeature
```


#### `saveAsImageFeatureDefault`

``` purescript
saveAsImageFeatureDefault :: SaveAsImageFeatureRec
```


#### `RestoreFeatureRec`

``` purescript
type RestoreFeatureRec = { title :: Maybe String, show :: Maybe Boolean }
```


#### `RestoreFeature`

``` purescript
newtype RestoreFeature
  = RestoreFeature RestoreFeatureRec
```


#### `restoreFeatureEncodeJson`

``` purescript
instance restoreFeatureEncodeJson :: EncodeJson RestoreFeature
```


#### `restoreFeatureDecodeJson`

``` purescript
instance restoreFeatureDecodeJson :: DecodeJson RestoreFeature
```


#### `restoreFeatureDefault`

``` purescript
restoreFeatureDefault :: RestoreFeatureRec
```


#### `DataZoomFeatureTitleRec`

``` purescript
type DataZoomFeatureTitleRec = { dataZoomReset :: String, dataZoom :: String }
```


#### `DataZoomFeatureTitle`

``` purescript
newtype DataZoomFeatureTitle
  = DataZoomFeatureTitle DataZoomFeatureTitleRec
```


#### `datazoomTitleEncodeJson`

``` purescript
instance datazoomTitleEncodeJson :: EncodeJson DataZoomFeatureTitle
```


#### `datazoomTitleDecodeJson`

``` purescript
instance datazoomTitleDecodeJson :: DecodeJson DataZoomFeatureTitle
```


#### `DataZoomFeatureRec`

``` purescript
type DataZoomFeatureRec = { title :: Maybe DataZoomFeatureTitle, show :: Maybe Boolean }
```


#### `DataZoomFeature`

``` purescript
newtype DataZoomFeature
  = DataZoomFeature DataZoomFeatureRec
```


#### `dataZoomFeatureDefault`

``` purescript
dataZoomFeatureDefault :: DataZoomFeatureRec
```


#### `dataZoomFeatureEncodeJson`

``` purescript
instance dataZoomFeatureEncodeJson :: EncodeJson DataZoomFeature
```


#### `dataZoomFeatureDecodeJson`

``` purescript
instance dataZoomFeatureDecodeJson :: DecodeJson DataZoomFeature
```


#### `DataViewFeatureRec`

``` purescript
type DataViewFeatureRec = { lang :: Maybe [String], readOnly :: Maybe Boolean, title :: Maybe String, show :: Maybe Boolean }
```


#### `DataViewFeature`

``` purescript
newtype DataViewFeature
  = DataViewFeature DataViewFeatureRec
```


#### `dataViewFeatureDefault`

``` purescript
dataViewFeatureDefault :: DataViewFeatureRec
```


#### `dataViewFeatureEncodeJson`

``` purescript
instance dataViewFeatureEncodeJson :: EncodeJson DataViewFeature
```


#### `dataViewFeatureDecodeJson`

``` purescript
instance dataViewFeatureDecodeJson :: DecodeJson DataViewFeature
```


#### `MarkFeatureTitleRec`

``` purescript
type MarkFeatureTitleRec = { markClear :: String, markUndo :: String, mark :: Maybe String }
```


#### `MarkFeatureTitle`

``` purescript
newtype MarkFeatureTitle
  = MarkFeatureTitle MarkFeatureTitleRec
```


#### `mftitleEncodeJson`

``` purescript
instance mftitleEncodeJson :: EncodeJson MarkFeatureTitle
```


#### `mftitleDecodeJson`

``` purescript
instance mftitleDecodeJson :: DecodeJson MarkFeatureTitle
```


#### `MarkFeatureRec`

``` purescript
type MarkFeatureRec = { lineStyle :: Maybe LineStyle, title :: Maybe MarkFeatureTitle, show :: Maybe Boolean }
```


#### `MarkFeature`

``` purescript
newtype MarkFeature
  = MarkFeature MarkFeatureRec
```


#### `markFeatureEncodeJson`

``` purescript
instance markFeatureEncodeJson :: EncodeJson MarkFeature
```


#### `markFeatureDecodeJson`

``` purescript
instance markFeatureDecodeJson :: DecodeJson MarkFeature
```


#### `markFeatureDefault`

``` purescript
markFeatureDefault :: MarkFeatureRec
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


#### `magicTypeEncodeJson`

``` purescript
instance magicTypeEncodeJson :: EncodeJson MagicType
```


#### `magicTypeDecodeJson`

``` purescript
instance magicTypeDecodeJson :: DecodeJson MagicType
```


#### `MagicTypeFeatureRec`

``` purescript
type MagicTypeFeatureRec = { "type" :: Maybe [MagicType], option :: Maybe Json, title :: Maybe (StrMap String), show :: Maybe Boolean }
```


#### `MagicTypeFeature`

``` purescript
newtype MagicTypeFeature
  = MagicTypeFeature MagicTypeFeatureRec
```


#### `magicTypeFeatureDefault`

``` purescript
magicTypeFeatureDefault :: MagicTypeFeatureRec
```


#### `magicTypeFeatureEncodeJson`

``` purescript
instance magicTypeFeatureEncodeJson :: EncodeJson MagicTypeFeature
```


#### `magicTypeFeatureDecodeJson`

``` purescript
instance magicTypeFeatureDecodeJson :: DecodeJson MagicTypeFeature
```



## Module ECharts.Tooltip

#### `TooltipTrigger`

``` purescript
data TooltipTrigger
  = TriggerItem 
  | TriggerAxis 
```


#### `tooltipTriggerEncodeJson`

``` purescript
instance tooltipTriggerEncodeJson :: EncodeJson TooltipTrigger
```


#### `tooltipTriggerDecodeJson`

``` purescript
instance tooltipTriggerDecodeJson :: DecodeJson TooltipTrigger
```


#### `TooltipPosition`

``` purescript
data TooltipPosition
  = Fixed [Number]
  | FuncPos ([Number] -> [Number])
```


#### `tooltipPositionEncodeJson`

``` purescript
instance tooltipPositionEncodeJson :: EncodeJson TooltipPosition
```


#### `tooltipPositionDecodeJson`

``` purescript
instance tooltipPositionDecodeJson :: DecodeJson TooltipPosition
```


#### `TooltipAxisPointerType`

``` purescript
data TooltipAxisPointerType
  = LinePointer 
  | CrossPointer 
  | ShadowPointer 
  | NonePointer 
```


#### `tooltipAxisPointerTypeEncodeJson`

``` purescript
instance tooltipAxisPointerTypeEncodeJson :: EncodeJson TooltipAxisPointerType
```


#### `tooltiopAxisPointerTypeDecodeJson`

``` purescript
instance tooltiopAxisPointerTypeDecodeJson :: DecodeJson TooltipAxisPointerType
```


#### `TooltipAxisPointerRec`

``` purescript
type TooltipAxisPointerRec = { shadowStyle :: Maybe AreaStyle, crossStyle :: Maybe LineStyle, lineStyle :: Maybe LineStyle, "type" :: Maybe TooltipAxisPointerType }
```


#### `TooltipAxisPointer`

``` purescript
newtype TooltipAxisPointer
  = TooltipAxisPointer TooltipAxisPointerRec
```


#### `tooltipAxisPointerEncodeJson`

``` purescript
instance tooltipAxisPointerEncodeJson :: EncodeJson TooltipAxisPointer
```


#### `tooltipAxisPointerDecodeJson`

``` purescript
instance tooltipAxisPointerDecodeJson :: DecodeJson TooltipAxisPointer
```


#### `tooltipAxisPointerDefault`

``` purescript
tooltipAxisPointerDefault :: TooltipAxisPointerRec
```


#### `TooltipRec`

``` purescript
type TooltipRec = { enterable :: Maybe Boolean, textStyle :: Maybe TextStyle, axisPointer :: Maybe TooltipAxisPointer, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderRadius :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, transitionDuration :: Maybe Number, hideDelay :: Maybe Number, showDelay :: Maybe Number, islandFormatter :: Maybe Formatter, formatter :: Maybe Formatter, position :: Maybe TooltipPosition, trigger :: Maybe TooltipTrigger, showContent :: Maybe Boolean, show :: Maybe Boolean }
```


#### `Tooltip`

``` purescript
newtype Tooltip
  = Tooltip TooltipRec
```


#### `tooltipEncodeJson`

``` purescript
instance tooltipEncodeJson :: EncodeJson Tooltip
```


#### `tooltipDecodeJson`

``` purescript
instance tooltipDecodeJson :: DecodeJson Tooltip
```


#### `tooltipDefault`

``` purescript
tooltipDefault :: TooltipRec
```



## Module ECharts.Utils

#### `unnull`

``` purescript
unnull :: Json -> Json
```


## Module ECharts.Item.Data

#### `ItemDataDatRec`

``` purescript
type ItemDataDatRec = { selected :: Maybe Boolean, itemStyle :: Maybe ItemStyle, tooltip :: Maybe Tooltip, name :: Maybe String, value :: ItemValue }
```


#### `ItemData`

``` purescript
data ItemData
  = Value ItemValue
  | Dat ItemDataDatRec
  | Label String
```


#### `itemDataEncodeJson`

``` purescript
instance itemDataEncodeJson :: EncodeJson ItemData
```


#### `itemDataDecodeJson`

``` purescript
instance itemDataDecodeJson :: DecodeJson ItemData
```


#### `dataDefault`

``` purescript
dataDefault :: ItemValue -> ItemDataDatRec
```



## Module ECharts.Item.Value

#### `XYRRec`

``` purescript
type XYRRec = { r :: Maybe Number, y :: Number, x :: Number }
```


#### `HLOCRec`

``` purescript
type HLOCRec = { c :: Number, o :: Number, l :: Number, h :: Number }
```


#### `ItemValue`

``` purescript
data ItemValue
  = None 
  | Simple Number
  | Many [Number]
  | XYR XYRRec
  | HLOC HLOCRec
```


#### `itemValueEncodeJson`

``` purescript
instance itemValueEncodeJson :: EncodeJson ItemValue
```


#### `itemValueDecodeJson`

``` purescript
instance itemValueDecodeJson :: DecodeJson ItemValue
```



## Module ECharts.Mark.Data

#### `MarkPointDataRec`

``` purescript
type MarkPointDataRec = { "type" :: Maybe String, yAxis :: Maybe Number, xAxis :: Maybe Number, y :: Maybe Number, x :: Maybe Number, value :: Maybe Number, name :: Maybe String }
```


#### `MarkPointData`

``` purescript
newtype MarkPointData
  = MarkPointData MarkPointDataRec
```


#### `mpDataEncodeJson`

``` purescript
instance mpDataEncodeJson :: EncodeJson MarkPointData
```


#### `mpDataDecodeJson`

``` purescript
instance mpDataDecodeJson :: DecodeJson MarkPointData
```


#### `markPointDataDefault`

``` purescript
markPointDataDefault :: MarkPointDataRec
```



## Module ECharts.Mark.Effect

#### `MarkPointEffectRec`

``` purescript
type MarkPointEffectRec = { shadowBlur :: Maybe Number, color :: Maybe Color, scaleSize :: Maybe Boolean, period :: Maybe Boolean, loop :: Maybe Boolean, show :: Maybe Boolean }
```


#### `MarkPointEffect`

``` purescript
newtype MarkPointEffect
  = MarkPointEffect MarkPointEffectRec
```


#### `mpEffectEncodeJson`

``` purescript
instance mpEffectEncodeJson :: EncodeJson MarkPointEffect
```


#### `mpEffectDecodeJson`

``` purescript
instance mpEffectDecodeJson :: DecodeJson MarkPointEffect
```


#### `markPointEffectDefault`

``` purescript
markPointEffectDefault :: MarkPointEffectRec
```



## Module ECharts.Mark.Line

#### `MarkLineRec`

``` purescript
type MarkLineRec = { itemStyle :: Maybe ItemStyle, "data" :: Maybe [Tuple MarkPointData MarkPointData], geoCoord :: Maybe [GeoCoord], effect :: Maybe MarkPointEffect, symbolRotate :: Maybe (Tuple Number Number), symbolSize :: Maybe DoubleSymbolSize, symbol :: Maybe (Tuple Symbol Symbol) }
```


#### `MarkLine`

``` purescript
newtype MarkLine
  = MarkLine MarkLineRec
```


#### `mlEncodeJson`

``` purescript
instance mlEncodeJson :: EncodeJson MarkLine
```


#### `mlDecodeJson`

``` purescript
instance mlDecodeJson :: DecodeJson MarkLine
```


#### `markLineDefault`

``` purescript
markLineDefault :: MarkLineRec
```


#### `addMarkLine`

``` purescript
addMarkLine :: forall e a. MarkLine -> EChart -> Eff (addMarkLineECharts :: ADD_MARKLINE | e) EChart
```


#### `delMarkLine`

``` purescript
delMarkLine :: forall e. Number -> String -> EChart -> Eff (removeMarkLine :: REMOVE_MARKLINE | e) EChart
```



## Module ECharts.Mark.Point

#### `MarkPointRec`

``` purescript
type MarkPointRec = { geoCoord :: Maybe (StrMap (Tuple Number Number)), "data" :: Maybe [MarkPointData], effect :: Maybe MarkPointEffect, large :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol }
```


#### `MarkPoint`

``` purescript
newtype MarkPoint
  = MarkPoint MarkPointRec
```


#### `markPointEncodeJson`

``` purescript
instance markPointEncodeJson :: EncodeJson MarkPoint
```


#### `markPointDecodeJson`

``` purescript
instance markPointDecodeJson :: DecodeJson MarkPoint
```


#### `markPointDefault`

``` purescript
markPointDefault :: MarkPointRec
```


#### `delMarkPoint`

``` purescript
delMarkPoint :: forall e. Number -> String -> EChart -> Eff (removeMarkPointECharts :: REMOVE_MARKPOINT | e) EChart
```


#### `addMarkPoint`

``` purescript
addMarkPoint :: forall e. MarkPoint -> EChart -> Eff (addMarkPointECharts :: ADD_MARKPOINT | e) EChart
```



## Module ECharts.Series.EventRiver

#### `EvolutionDetailRec`

``` purescript
type EvolutionDetailRec = { img :: Maybe String, text :: Maybe String, link :: Maybe String }
```


#### `EvolutionDetail`

``` purescript
newtype EvolutionDetail
  = EvolutionDetail EvolutionDetailRec
```


#### `evoDetailEncodeJson`

``` purescript
instance evoDetailEncodeJson :: EncodeJson EvolutionDetail
```


#### `evoDetailDecodeJson`

``` purescript
instance evoDetailDecodeJson :: DecodeJson EvolutionDetail
```


#### `evolutionDetailDefault`

``` purescript
evolutionDetailDefault :: EvolutionDetailRec
```


#### `EvolutionRec`

``` purescript
type EvolutionRec = { detail :: Maybe EvolutionDetail, value :: Number, time :: Date }
```


#### `Evolution`

``` purescript
newtype Evolution
  = Evolution EvolutionRec
```


#### `evoEncodeJson`

``` purescript
instance evoEncodeJson :: EncodeJson Evolution
```


#### `evoDecodeJson`

``` purescript
instance evoDecodeJson :: DecodeJson Evolution
```


#### `OneEventRec`

``` purescript
type OneEventRec = { evolution :: Maybe [Evolution], weight :: Maybe Number, name :: Maybe String }
```


#### `OneEvent`

``` purescript
newtype OneEvent
  = OneEvent OneEventRec
```


#### `oneEventDefault`

``` purescript
oneEventDefault :: OneEventRec
```


#### `oneEventEncodeJson`

``` purescript
instance oneEventEncodeJson :: EncodeJson OneEvent
```


#### `oneEventDecodeJson`

``` purescript
instance oneEventDecodeJson :: DecodeJson OneEvent
```



## Module ECharts.Series.Force

#### `ForceCategoryRec`

``` purescript
type ForceCategoryRec = { itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, name :: Maybe String }
```


#### `ForceCategory`

``` purescript
newtype ForceCategory
  = ForceCategory ForceCategoryRec
```


#### `forceCategoryDefault`

``` purescript
forceCategoryDefault :: ForceCategoryRec
```


#### `forceCategoryEncodeJson`

``` purescript
instance forceCategoryEncodeJson :: EncodeJson ForceCategory
```


#### `forceCategoryDecodeJson`

``` purescript
instance forceCategoryDecodeJson :: DecodeJson ForceCategory
```


#### `NodeRec`

``` purescript
type NodeRec = { category :: Maybe Number, draggable :: Maybe Boolean, fixY :: Maybe Boolean, fixX :: Maybe Boolean, initial :: Maybe (Tuple Number Number), itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, ignore :: Maybe Boolean, value :: Number, label :: Maybe String, name :: Maybe String }
```


#### `Node`

``` purescript
newtype Node
  = Node NodeRec
```


#### `nodeDefault`

``` purescript
nodeDefault :: Number -> NodeRec
```


#### `nodeEncodeJson`

``` purescript
instance nodeEncodeJson :: EncodeJson Node
```


#### `nodeDecodeJson`

``` purescript
instance nodeDecodeJson :: DecodeJson Node
```


#### `LinkEnd`

``` purescript
data LinkEnd
  = Name String
  | Index Number
```


#### `linkEndEncodeJson`

``` purescript
instance linkEndEncodeJson :: EncodeJson LinkEnd
```


#### `linkEndDecodeJson`

``` purescript
instance linkEndDecodeJson :: DecodeJson LinkEnd
```


#### `LinkRec`

``` purescript
type LinkRec = { itemStyle :: Maybe ItemStyle, weight :: Number, target :: LinkEnd, source :: LinkEnd }
```


#### `Link`

``` purescript
newtype Link
  = Link LinkRec
```


#### `linkEncodeJson`

``` purescript
instance linkEncodeJson :: EncodeJson Link
```


#### `linkDecodeJson`

``` purescript
instance linkDecodeJson :: DecodeJson Link
```


#### `Matrix`

``` purescript
type Matrix = [[Number]]
```



## Module ECharts.Series.Gauge

#### `PointerRec`

``` purescript
type PointerRec = { color :: Maybe Color, width :: Maybe Number, length :: Maybe Number }
```


#### `Pointer`

``` purescript
newtype Pointer
  = Pointer PointerRec
```


#### `pointerDefault`

``` purescript
pointerDefault :: PointerRec
```


#### `pointerEncodeJson`

``` purescript
instance pointerEncodeJson :: EncodeJson Pointer
```


#### `pointerDecodeJson`

``` purescript
instance pointerDecodeJson :: DecodeJson Pointer
```


#### `SplitLineRec`

``` purescript
type SplitLineRec = { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean }
```


#### `SplitLine`

``` purescript
newtype SplitLine
  = SplitLine SplitLineRec
```


#### `splitLineDefault`

``` purescript
splitLineDefault :: SplitLineRec
```


#### `splitLineEncodeJson`

``` purescript
instance splitLineEncodeJson :: EncodeJson SplitLine
```


#### `splitLineDecodeJson`

``` purescript
instance splitLineDecodeJson :: DecodeJson SplitLine
```


#### `GaugeDetailRec`

``` purescript
type GaugeDetailRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, offsetCenter :: Maybe (Tuple PercentOrPixel PercentOrPixel), height :: Maybe Number, width :: Maybe Number, borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, show :: Maybe Boolean }
```


#### `GaugeDetail`

``` purescript
newtype GaugeDetail
  = GaugeDetail GaugeDetailRec
```


#### `gaugeDetailDefault`

``` purescript
gaugeDetailDefault :: GaugeDetailRec
```


#### `gaugeDetailEncodeJson`

``` purescript
instance gaugeDetailEncodeJson :: EncodeJson GaugeDetail
```


#### `gaugeDetailDecodeJson`

``` purescript
instance gaugeDetailDecodeJson :: DecodeJson GaugeDetail
```



## Module ECharts.Style.Area

#### `AreaStyle`

``` purescript
newtype AreaStyle
  = AreaStyle Color
```


#### `areaStyleEncodeJson`

``` purescript
instance areaStyleEncodeJson :: EncodeJson AreaStyle
```


#### `areaStyleDecodeJson`

``` purescript
instance areaStyleDecodeJson :: DecodeJson AreaStyle
```



## Module ECharts.Style.Checkpoint

#### `CheckpointStyleRec`

``` purescript
type CheckpointStyleRec = { label :: Maybe AxisLabel, borderColor :: Maybe Color, color :: Maybe Color, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol }
```


#### `CheckpointStyle`

``` purescript
newtype CheckpointStyle
  = CheckpointStyle CheckpointStyleRec
```


#### `checkpointStyleEncodeJson`

``` purescript
instance checkpointStyleEncodeJson :: EncodeJson CheckpointStyle
```


#### `checkpointStyleDecodeJson`

``` purescript
instance checkpointStyleDecodeJson :: DecodeJson CheckpointStyle
```


#### `checkpointStyleDefault`

``` purescript
checkpointStyleDefault :: CheckpointStyleRec
```



## Module ECharts.Style.Chord

#### `ChordStyleRec`

``` purescript
type ChordStyleRec = { borderColor :: Maybe Color, borderWidth :: Maybe Number, color :: Maybe Color, width :: Maybe Number }
```


#### `ChordStyle`

``` purescript
newtype ChordStyle
  = ChordStyle ChordStyleRec
```


#### `chordStyleJson`

``` purescript
instance chordStyleJson :: EncodeJson ChordStyle
```


#### `chordStyleDecodeJson`

``` purescript
instance chordStyleDecodeJson :: DecodeJson ChordStyle
```


#### `chordStyleDefault`

``` purescript
chordStyleDefault :: ChordStyleRec
```



## Module ECharts.Style.Item

#### `ItemLabelRec`

``` purescript
type ItemLabelRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, distance :: Maybe Boolean, rotate :: Maybe Boolean, position :: Maybe LabelPosition, show :: Maybe Boolean }
```


#### `ItemLabel`

``` purescript
newtype ItemLabel
  = ItemLabel ItemLabelRec
```


#### `itemLabelEncodeJson`

``` purescript
instance itemLabelEncodeJson :: EncodeJson ItemLabel
```


#### `itemLabelDecodeJson`

``` purescript
instance itemLabelDecodeJson :: DecodeJson ItemLabel
```


#### `itemLabelDefault`

``` purescript
itemLabelDefault :: ItemLabelRec
```


#### `ItemLabelLineRec`

``` purescript
type ItemLabelLineRec = { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean }
```


#### `ItemLabelLine`

``` purescript
newtype ItemLabelLine
  = ItemLabelLine ItemLabelLineRec
```


#### `itemLabelLineEncodeJson`

``` purescript
instance itemLabelLineEncodeJson :: EncodeJson ItemLabelLine
```


#### `itemLabelLineDecodeJson`

``` purescript
instance itemLabelLineDecodeJson :: DecodeJson ItemLabelLine
```


#### `itemLabelLineDefault`

``` purescript
itemLabelLineDefault :: ItemLabelLineRec
```


#### `IStyleRec`

``` purescript
type IStyleRec = { linkStyle :: Maybe LinkStyle, nodeStyle :: Maybe NodeStyle, chordStyle :: Maybe ChordStyle, areaStyle :: Maybe AreaStyle, lineStyle :: Maybe LineStyle, labelLine :: Maybe ItemLabelLine, label :: Maybe ItemLabel, barBorderWidth :: Maybe Number, barBorderRadius :: Maybe (Corner Number), barBorderColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe CalculableColor }
```


#### `IStyle`

``` purescript
newtype IStyle
  = IStyle IStyleRec
```


#### `istyleDefault`

``` purescript
istyleDefault :: IStyleRec
```


#### `istyleEncodeJson`

``` purescript
instance istyleEncodeJson :: EncodeJson IStyle
```


#### `istyleDecodeJson`

``` purescript
instance istyleDecodeJson :: DecodeJson IStyle
```


#### `ItemStyleRec`

``` purescript
type ItemStyleRec = { emphasis :: Maybe IStyle, normal :: Maybe IStyle }
```


#### `ItemStyle`

``` purescript
newtype ItemStyle
  = ItemStyle ItemStyleRec
```


#### `itemStyleEncodeJson`

``` purescript
instance itemStyleEncodeJson :: EncodeJson ItemStyle
```


#### `itemStyleDecodeJson`

``` purescript
instance itemStyleDecodeJson :: DecodeJson ItemStyle
```


#### `itemStyleDefault`

``` purescript
itemStyleDefault :: ItemStyleRec
```



## Module ECharts.Style.Line

#### `LineType`

``` purescript
data LineType
  = Solid 
  | Dotted 
  | Dashed 
```


#### `linetypeEncodeJson`

``` purescript
instance linetypeEncodeJson :: EncodeJson LineType
```


#### `linetypeDecodeJson`

``` purescript
instance linetypeDecodeJson :: DecodeJson LineType
```


#### `LineStyleRec`

``` purescript
type LineStyleRec = { shadowOffsetY :: Maybe Number, shadowOffsetX :: Maybe Number, shadowColor :: Maybe Color, width :: Maybe Number, "type" :: Maybe LineType, color :: Maybe Color }
```


#### `LineStyle`

``` purescript
newtype LineStyle
  = LineStyle LineStyleRec
```


#### `lineStyleEncodeJson`

``` purescript
instance lineStyleEncodeJson :: EncodeJson LineStyle
```


#### `lineStyleDecodeJson`

``` purescript
instance lineStyleDecodeJson :: DecodeJson LineStyle
```


#### `lineStyleDefault`

``` purescript
lineStyleDefault :: LineStyleRec
```



## Module ECharts.Style.Link

#### `LinkType`

``` purescript
data LinkType
  = LTCurve 
  | LTLine 
```


#### `linkTypeEncodeJson`

``` purescript
instance linkTypeEncodeJson :: EncodeJson LinkType
```


#### `linkTypeDecodeJson`

``` purescript
instance linkTypeDecodeJson :: DecodeJson LinkType
```


#### `LinkStyleRec`

``` purescript
type LinkStyleRec = { width :: Maybe Number, color :: Maybe Color, "type" :: Maybe LinkType }
```


#### `LinkStyle`

``` purescript
newtype LinkStyle
  = LinkStyle LinkStyleRec
```


#### `linkStyleEncodeJson`

``` purescript
instance linkStyleEncodeJson :: EncodeJson LinkStyle
```


#### `linkStyleDecodeJson`

``` purescript
instance linkStyleDecodeJson :: DecodeJson LinkStyle
```


#### `linkStyleDefault`

``` purescript
linkStyleDefault :: LinkStyleRec
```



## Module ECharts.Style.Node

#### `NodeStyleRec`

``` purescript
type NodeStyleRec = { borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe Color }
```


#### `NodeStyle`

``` purescript
newtype NodeStyle
  = NodeStyle NodeStyleRec
```


#### `nodeStyleEncodeJson`

``` purescript
instance nodeStyleEncodeJson :: EncodeJson NodeStyle
```


#### `nodeStyleDecodeJson`

``` purescript
instance nodeStyleDecodeJson :: DecodeJson NodeStyle
```


#### `nodeStyleDefault`

``` purescript
nodeStyleDefault :: NodeStyleRec
```



## Module ECharts.Style.Text

#### `Decoration`

``` purescript
type Decoration = String
```


#### `FontFamily`

``` purescript
type FontFamily = String
```


#### `TextBaseline`

``` purescript
data TextBaseline
  = TBLTop 
  | TBLBottom 
  | TBLMiddle 
```


#### `textBaselineEncodeJson`

``` purescript
instance textBaselineEncodeJson :: EncodeJson TextBaseline
```


#### `textBaselineDecodeJson`

``` purescript
instance textBaselineDecodeJson :: DecodeJson TextBaseline
```


#### `FontStyle`

``` purescript
data FontStyle
  = FSNormal 
  | FSItalic 
  | FSOblique 
```


#### `fontStyleEncodeJson`

``` purescript
instance fontStyleEncodeJson :: EncodeJson FontStyle
```


#### `fontStyleDecodeJson`

``` purescript
instance fontStyleDecodeJson :: DecodeJson FontStyle
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


#### `fontWeightEncodeJson`

``` purescript
instance fontWeightEncodeJson :: EncodeJson FontWeight
```


#### `fontWeightDecodeJson`

``` purescript
instance fontWeightDecodeJson :: DecodeJson FontWeight
```


#### `TextStyleRec`

``` purescript
type TextStyleRec = { fontWeight :: Maybe FontWeight, fontStyle :: Maybe FontStyle, fontSize :: Maybe Number, fontFamily :: Maybe FontFamily, baseline :: Maybe TextBaseline, align :: Maybe HorizontalAlign, decoration :: Maybe Decoration, color :: Maybe Color }
```


#### `TextStyle`

``` purescript
newtype TextStyle
  = TextStyle TextStyleRec
```


#### `textStyleEncodeJson`

``` purescript
instance textStyleEncodeJson :: EncodeJson TextStyle
```


#### `textStyleDecodeJson`

``` purescript
instance textStyleDecodeJson :: DecodeJson TextStyle
```


#### `textStyleDefault`

``` purescript
textStyleDefault :: TextStyleRec
```




