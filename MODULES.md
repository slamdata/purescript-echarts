# Module Documentation

## Module ECharts.AddData

### Types

    newtype AdditionalData where
      AdditionalData :: AdditionalDataRec -> AdditionalData

    type AdditionalDataRec = { additionalData :: Maybe String, dataGrow :: Boolean, isHead :: Boolean, datum :: ItemData, idx :: Number }


### Type Class Instances

    instance additionalDataEncodeJson :: EncodeJson AdditionalData


### Values

    addData :: forall e. AdditionalData -> EChart -> Eff (dataAdd :: AddData | e) EChart


## Module ECharts.Axis

### Types

    newtype Axis where
      Axis :: AxisRec -> Axis

    data AxisBoundaryGap where
      CatBoundaryGap :: Boolean -> AxisBoundaryGap
      ValueBoundaryGap :: Number -> Number -> AxisBoundaryGap

    data AxisData where
      CommonAxisData :: String -> AxisData
      CustomAxisData :: CustomAxisDataRec -> AxisData

    newtype AxisLabel where
      AxisLabel :: AxisLabelRec -> AxisLabel

    type AxisLabelRec = { clickable :: Maybe Boolean, margin :: Maybe Number, rotate :: Maybe Number, textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, interval :: Maybe Interval, show :: Maybe Boolean }

    newtype AxisLine where
      AxisLine :: AxisLineRec -> AxisLine

    type AxisLineRec = { lineStyle :: Maybe AxisLineStyle, onZero :: Maybe Boolean, show :: Maybe Boolean }

    newtype AxisLineStyle where
      AxisLineStyle :: AxisLineStyleRec -> AxisLineStyle

    type AxisLineStyleRec = { width :: Maybe Number, color :: Maybe [Tuple Number Color] }

    data AxisNameLocation where
      Start :: AxisNameLocation
      End :: AxisNameLocation

    data AxisPosition where
      LeftAxis :: AxisPosition
      RightAxis :: AxisPosition
      TopAxis :: AxisPosition
      BottomAxis :: AxisPosition

    type AxisRec = { "data" :: Maybe [AxisData], splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisTick :: Maybe AxisTick, axisLine :: Maybe AxisLine, splitNumber :: Maybe Number, scale :: Maybe Boolean, max :: Maybe Number, min :: Maybe Number, boundaryGap :: Maybe AxisBoundaryGap, nameTextStyle :: Maybe TextStyle, nameLocation :: Maybe AxisNameLocation, name :: Maybe String, position :: Maybe AxisPosition, show :: Maybe Boolean, "type" :: Maybe AxisType }

    newtype AxisSplitArea where
      AxisSplitArea :: AxisSplitAreaRec -> AxisSplitArea

    type AxisSplitAreaRec = { areaStyle :: Maybe AreaStyle, onGap :: Maybe Boolean, show :: Maybe Boolean }

    newtype AxisSplitLine where
      AxisSplitLine :: AxisSplitLineRec -> AxisSplitLine

    type AxisSplitLineRec = { lineStyle :: Maybe LineStyle, onGap :: Maybe Boolean, show :: Maybe Boolean }

    newtype AxisTick where
      AxisTick :: AxisTickRec -> AxisTick

    type AxisTickRec = { inside :: Maybe Boolean, onGap :: Maybe Boolean, interval :: Maybe Interval, lineStyle :: Maybe LineStyle, length :: Maybe Number, splitNumber :: Maybe Number, show :: Maybe Boolean }

    data AxisType where
      CategoryAxis :: AxisType
      ValueAxis :: AxisType
      TimeAxis :: AxisType

    data Axises where
      OneAxis :: Axis -> Axises
      TwoAxises :: Axis -> Axis -> Axises

    type CustomAxisDataRec = { textStyle :: TextStyle, value :: String }

    newtype Indicator where
      Indicator :: IndicatorRec -> Indicator

    type IndicatorRec = { axisLabel :: Maybe AxisLabel, max :: Maybe Number, min :: Maybe Number, text :: Maybe String }

    newtype Polar where
      Polar :: PolarRec -> Polar

    newtype PolarName where
      PolarName :: PolarNameRec -> PolarName

    type PolarNameRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, show :: Maybe Boolean }

    type PolarRec = { indicator :: Maybe [Indicator], "type" :: Maybe PolarType, splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisLine :: Maybe AxisLine, scale :: Maybe Boolean, boundaryGap :: Maybe (Tuple Number Number), name :: Maybe PolarName, splitNumber :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe PercentOrPixel, center :: Maybe (Tuple PercentOrPixel PercentOrPixel) }

    data PolarType where
      PolarPolygon :: PolarType
      PolarCircle :: PolarType


### Type Class Instances

    instance axisBoundaryGapEncodeJson :: EncodeJson AxisBoundaryGap

    instance axisDataEncodeJson :: EncodeJson AxisData

    instance axisEncJson :: EncodeJson Axis

    instance axisLabelEncodeJson :: EncodeJson AxisLabel

    instance axisLineEncodeJson :: EncodeJson AxisLine

    instance axisLineStyleEncodeJson :: EncodeJson AxisLineStyle

    instance axisNameLocationEncodeJson :: EncodeJson AxisNameLocation

    instance axisPositionEncodeJson :: EncodeJson AxisPosition

    instance axisSplitAreaEncodeJson :: EncodeJson AxisSplitArea

    instance axisSplitLineEncodeJson :: EncodeJson AxisSplitLine

    instance axisTickEncodeJson :: EncodeJson AxisTick

    instance axisTypeEncodeJson :: EncodeJson AxisType

    instance axisesEncodeJson :: EncodeJson Axises

    instance indicatorEncodeJson :: EncodeJson Indicator

    instance polarEncodeJson :: EncodeJson Polar

    instance polarNameEncode :: EncodeJson PolarName

    instance polarTypeEncode :: EncodeJson PolarType


### Values

    axisDefault :: AxisRec

    axisLabelDefault :: AxisLabelRec

    axisLineDefault :: AxisLineRec

    axisLineStyleDefault :: AxisLineStyleRec

    axisSplitAreaDefault :: AxisSplitAreaRec

    axisSplitLineDefault :: AxisSplitLineRec

    axisTickDefault :: AxisTickRec

    indicatorDefault :: IndicatorRec

    polarDefault :: PolarRec

    polarNameDefault :: PolarNameRec


## Module ECharts.Chart

### Types

    data EChart :: *

    data Theme where
      ThemeName :: String -> Theme
      ThemeConfig :: Json -> Theme

    data ZRender :: *


### Type Class Instances

    instance themeEncodeJson :: EncodeJson Theme


### Values

    clear :: forall e. EChart -> Eff (echartClear :: EChartClear, dom :: DOM | e) Unit

    dispose :: forall e. EChart -> Eff (echartDispose :: EChartDispose, dom :: DOM | e) Unit

    getZRender :: forall e. EChart -> Eff e ZRender

    init :: forall e. Maybe Theme -> HTMLElement -> Eff (echartInit :: EChartInit, dom :: DOM | e) EChart

    refresh :: forall e. EChart -> Eff (echartRefresh :: EChartRefresh, dom :: DOM | e) Unit

    resize :: forall e. EChart -> Eff (echartResize :: EChartResize, dom :: DOM | e) Unit

    setTheme :: forall e. Theme -> EChart -> Eff (echartTheme :: EChartThemeSet, dom :: DOM | e) EChart


## Module ECharts.Color

### Types

    data CalculableColor where
      SimpleColor :: Color -> CalculableColor
      ColorFunc :: (ColorFuncParam -> Color) -> CalculableColor

    type Color = String

    newtype ColorFuncParam where
      ColorFuncParam :: ColorFuncParamRec -> ColorFuncParam

    type ColorFuncParamRec = { "data" :: { name :: String, value :: ItemValue }, dataIndex :: Number, series :: String, seriesIndex :: Number }


### Type Class Instances

    instance calculableColorEncodeJson :: EncodeJson CalculableColor


### Values


## Module ECharts.Common

### Types

    type Center = Tuple PercentOrPixel PercentOrPixel

    data Corner a where
      AllCorners :: a -> Corner a
      Corners :: a -> a -> a -> a -> Corner a

    type GeoCoord = M.StrMap (Tuple Number Number)

    data Interval where
      Auto :: Interval
      Custom :: Number -> Interval

    data MapValueCalculation where
      SumCalculation :: MapValueCalculation
      AverageCalculation :: MapValueCalculation

    newtype MinMax where
      MinMax :: MinMaxRec -> MinMax

    type MinMaxRec = { max :: Number, min :: Number }

    data PercentOrPixel where
      Percent :: Number -> PercentOrPixel
      Pixel :: Number -> PercentOrPixel

    data Radius where
      R :: PercentOrPixel -> Radius
      Rs :: RsRec -> Radius

    data Roam where
      Enable :: Roam
      Disable :: Roam
      Scale :: Roam
      Move :: Roam

    data RoseType where
      RTRadius :: RoseType
      RTArea :: RoseType

    type RsRec = { outer :: PercentOrPixel, inner :: PercentOrPixel }

    data SelectedMode where
      SelModeSingle :: SelectedMode
      SelModeMultiple :: SelectedMode
      SelModeFalse :: SelectedMode

    data Sort where
      NoSort :: Sort
      Asc :: Sort
      Desc :: Sort


### Type Class Instances

    instance cornerJsonEncode :: (EncodeJson a) => EncodeJson (Corner a)

    instance intervalEncodeJson :: EncodeJson Interval

    instance mapValueCalculationEncodeJson :: EncodeJson MapValueCalculation

    instance minMaxEncodeJson :: EncodeJson MinMax

    instance percentOrPixelEncodeJson :: EncodeJson PercentOrPixel

    instance radiusEncodeJson :: EncodeJson Radius

    instance roamEncodeJson :: EncodeJson Roam

    instance roseTypeEncodeJson :: EncodeJson RoseType

    instance selModeEncodeJson :: EncodeJson SelectedMode

    instance sortEncodeJson :: EncodeJson Sort


## Module ECharts.Connect

### Types

    newtype Connection


### Values

    connect :: forall e. EChart -> EChart -> Eff (connect :: Connect | e) Connection


## Module ECharts.Coords

### Types

    data HorizontalAlign where
      HAlignLeft :: HorizontalAlign
      HAlignRight :: HorizontalAlign
      HAlignCenter :: HorizontalAlign

    data LabelPosition where
      LPOuter :: LabelPosition
      LPInner :: LabelPosition
      LPTop :: LabelPosition
      LPRight :: LabelPosition
      LPLeft :: LabelPosition
      LPBottom :: LabelPosition
      LPInside :: LabelPosition
      LPInsideLeft :: LabelPosition
      LPInsideRight :: LabelPosition
      LPInsideTop :: LabelPosition
      LPInsideBottom :: LabelPosition

    newtype Location where
      Location :: LocationRec -> Location

    type LocationRec = { y :: Maybe YPos, x :: Maybe XPos }

    data Orient where
      Horizontal :: Orient
      Vertical :: Orient

    data XPos where
      XLeft :: XPos
      XRight :: XPos
      XCenter :: XPos
      X :: Number -> XPos

    data YPos where
      YTop :: YPos
      YBottom :: YPos
      YCenter :: YPos
      Y :: Number -> YPos


### Type Class Instances

    instance labelPositionEncodeJson :: EncodeJson LabelPosition

    instance locationEncodeJson :: EncodeJson Location

    instance orientEncodeJson :: EncodeJson Orient

    instance textAlignEncodeJson :: EncodeJson HorizontalAlign

    instance xPosEncodeJson :: EncodeJson XPos

    instance yPosEncodeJson :: EncodeJson YPos


## Module ECharts.DataRange

### Types

    newtype DataRange where
      DataRange :: DataRangeRec -> DataRange

    type DataRangeRec = { textStyle :: Maybe TextStyle, text :: Maybe (Tuple String String), formatter :: Maybe Formatter, color :: Maybe [Color], realtime :: Maybe Boolean, hoverLink :: Maybe Boolean, calculable :: Maybe Boolean, selectedMode :: Maybe SelectedMode, splitNumber :: Maybe Number, precision :: Maybe Number, max :: Maybe Number, min :: Maybe Number, itemHeight :: Maybe Number, itemWidth :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }


### Type Class Instances

    instance dataRangeEncodeJson :: EncodeJson DataRange


### Values

    dataRangeDefault :: DataRangeRec


## Module ECharts.DataZoom

### Types

    newtype DataZoom where
      DataZoom :: DataZoomRec -> DataZoom

    type DataZoomRec = { zoomlock :: Maybe Boolean, realtime :: Maybe Boolean, showDetail :: Maybe Boolean, end :: Maybe Number, start :: Maybe Number, yAxisIndex :: Maybe [Number], xAxisIndex :: Maybe [Number], handleColor :: Maybe Color, fillerColor :: Maybe Color, dataBackgroundColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }


### Type Class Instances

    instance dataZoomEncodeJson :: EncodeJson DataZoom


### Values

    dataZoomDefault :: DataZoomRec


## Module ECharts.Effects

### Types

    data AddData :: !

    data AddMarkLine :: !

    data AddMarkPoint :: !

    data Connect :: !

    data Disconnect :: !

    data EChartClear :: !

    data EChartDispose :: !

    data EChartInit :: !

    data EChartOptionSet :: !

    data EChartRefresh :: !

    data EChartResize :: !

    data EChartThemeSet :: !

    data ImageMaking :: !

    data Listen :: !

    data LoadingHide :: !

    data LoadingShow :: !

    data RemoveMarkLine :: !

    data RemoveMarkPoint :: !

    data Unlisten :: !


## Module ECharts.Events

### Types

    type EventParam = Json

    data EventType where
      Refresh :: EventType
      Restore :: EventType
      Resize :: EventType
      Click :: EventType
      DoubleClick :: EventType
      Hover :: EventType
      DataChanged :: EventType
      DataZoom :: EventType
      DataRange :: EventType
      DataRangeHoverLink :: EventType
      LegendSelected :: EventType
      LegendHoverLink :: EventType
      MapSelected :: EventType
      PieSelected :: EventType
      DataViewChanged :: EventType
      MapRoam :: EventType
      MagicTypeChanged :: EventType

    newtype Sub


### Values

    listen :: forall e. EventType -> (EventParam -> Eff (listen :: Listen | e) Unit) -> EChart -> Eff (listen :: Listen | e) Sub


## Module ECharts.Formatter

### Types

    type FormatParams = Json

    data Formatter where
      Template :: String -> Formatter
      FormatFunc :: (forall eff. [FormatParams] -> Eff eff String) -> Formatter


### Type Class Instances

    instance formatterEncodeJson :: EncodeJson Formatter


### Values


## Module ECharts.Grid

### Types

    newtype Grid where
      Grid :: GridRec -> Grid

    type GridRec = { borderColor :: Maybe Number, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel }


### Type Class Instances

    instance gridEncodeJson :: EncodeJson Grid


### Values

    gridDefault :: GridRec


## Module ECharts.Image

### Types

    data ImgType where
      PNG :: ImgType
      JPEG :: ImgType


### Type Class Instances

    instance encodeImg :: EncodeJson ImgType


### Values

    getDataURL :: forall e. ImgType -> EChart -> Eff (image :: ImageMaking | e) String

    getImage :: forall e. ImgType -> EChart -> Eff (image :: ImageMaking, dom :: DOM | e) Node


## Module ECharts.Legend

### Types

    newtype Legend where
      Legend :: LegendRec -> Legend

    data LegendItem where
      LegendItem :: String -> LegendItemRec -> LegendItem

    type LegendItemRec = { textStyle :: Maybe TextStyle, icon :: Maybe String }

    type LegendRec = { "data" :: Maybe [LegendItem], selected :: Maybe (StrMap Boolean), selectedMode :: Maybe SelectedMode, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle, itemWidth :: Maybe Number, itemHeight :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }


### Type Class Instances

    instance legendEncodeJson :: EncodeJson Legend

    instance legendItemEncodeJson :: EncodeJson LegendItem


### Values

    legendDefault :: LegendRec

    legendItemDefault :: String -> LegendItem


## Module ECharts.Loading

### Types

    data LoadingEffect where
      Spin :: LoadingEffect
      Bar :: LoadingEffect
      Ring :: LoadingEffect
      Whirling :: LoadingEffect
      DynamicLine :: LoadingEffect
      Bubble :: LoadingEffect

    newtype LoadingOption where
      LoadingOption :: LoadingOptionRec -> LoadingOption

    type LoadingOptionRec = { progress :: Maybe Number, effectOption :: Maybe Json, effect :: Maybe LoadingEffect, textStyle :: Maybe TextStyle, y :: Maybe YPos, x :: Maybe XPos, text :: Maybe String }


### Type Class Instances

    instance loadingEffectEncodeJson :: EncodeJson LoadingEffect

    instance showLoadingOptions :: EncodeJson LoadingOption


### Values

    hideLoading :: forall e. EChart -> Eff (hideLoadingECharts :: LoadingHide | e) EChart

    loadingOptionDefault :: LoadingOptionRec

    showLoading :: forall e. LoadingOption -> EChart -> Eff (showLoadingECharts :: LoadingShow | e) EChart


## Module ECharts.Options

### Types

    newtype Option where
      Option :: OptionRec -> Option

    type OptionRec = { series :: Maybe [Maybe Series], polar :: Maybe [Polar], yAxis :: Maybe Axises, xAxis :: Maybe Axises, grid :: Maybe Grid, roamController :: Maybe RoamController, dataZoom :: Maybe DataZoom, dataRange :: Maybe DataRange, legend :: Maybe Legend, title :: Maybe Title, toolbox :: Maybe Toolbox, tooltip :: Maybe Tooltip, timeline :: Maybe Timeline, animation :: Maybe Boolean, calculable :: Maybe Boolean, renderAsImage :: Maybe Boolean, color :: Maybe [Color], backgroundColor :: Maybe Color }


### Type Class Instances

    instance optionsEncodeJson :: EncodeJson Option


### Values

    optionDefault :: OptionRec

    setOption :: forall e. Option -> Boolean -> EChart -> Eff (echartSetOption :: EChartOptionSet | e) EChart


## Module ECharts.RoamController

### Types

    newtype RoamController where
      RoamController :: RoamControllerRec -> RoamController

    type RoamControllerRec = { mapTypeControl :: Maybe (StrMap Boolean), step :: Maybe Number, handleColor :: Maybe Color, fillerColor :: Maybe Color, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, show :: Maybe Boolean }


### Type Class Instances

    instance roamControllerEncodeJson :: EncodeJson RoamController


### Values

    roamControllerDefault :: RoamControllerRec


## Module ECharts.Series

### Types

    type BarSeriesRec = { legendHoverLink :: Maybe Boolean, barMaxWidth :: Maybe Number, barWidth :: Maybe Number, barMinHeight :: Maybe Number, barCategoryGap :: Maybe PercentOrPixel, barGap :: Maybe PercentOrPixel, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, stack :: Maybe String, "data" :: Maybe [ItemData] }

    type CandlestickSeriesRec = { barMaxWidth :: Maybe Number, barWidth :: Maybe Number, barMinHeight :: Maybe Number, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, "data" :: Maybe [ItemData] }

    type ChordSeriesRec = { clockWise :: Maybe Boolean, sortSub :: Maybe Sort, sort :: Maybe Sort, padding :: Maybe Number, showScaleText :: Maybe Boolean, showScale :: Maybe Boolean, maxRadius :: Maybe Number, minRadius :: Maybe Number, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, ribbonType :: Maybe Boolean, "data" :: Maybe [ItemData], matrix :: Maybe Matrix, links :: Maybe [Link], categories :: Maybe [ForceCategory], nodes :: Maybe [Node] }

    type EventRiverSeriesRec = { legendHoverLink :: Maybe Boolean, weight :: Maybe Number, xAxisIndex :: Maybe Number, eventList :: Maybe [OneEvent] }

    type ForceSeriesRec = { ribbonType :: Maybe Boolean, steps :: Maybe Number, useWorker :: Maybe Boolean, large :: Maybe Boolean, draggable :: Maybe Number, gravity :: Maybe Number, scaling :: Maybe Number, linkSymbolSize :: Maybe Symbol, linkSymbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, maxRadius :: Maybe Number, minRadius :: Maybe Number, size :: Maybe Number, center :: Maybe Center, matrix :: Maybe Matrix, links :: Maybe [Link], nodes :: Maybe [Node], categories :: Maybe [ForceCategory] }

    type FunnelSeriesRec = { legendHoverLink :: Maybe Boolean, sort :: Maybe Sort, gap :: Maybe Number, maxSize :: Maybe PercentOrPixel, minSize :: Maybe PercentOrPixel, max :: Maybe Number, min :: Maybe Number, funnelAlign :: Maybe HorizontalAlign, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel, "data" :: Maybe [ItemData] }

    type GaugeSeriesRec = { legendHoverLink :: Maybe Boolean, pointer :: Maybe Pointer, detail :: Maybe GaugeDetail, title :: Maybe Title, splitLine :: Maybe SplitLine, axisLabel :: Maybe AxisLabel, axisTick :: Maybe AxisTick, axisLine :: Maybe AxisLine, splitNumber :: Maybe Number, precision :: Maybe Number, max :: Maybe Number, min :: Maybe Number, endAngle :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe Radius, center :: Maybe (Tuple Number Number), "data" :: Maybe [ItemData] }

    type LineSeriesRec = { legendHoverLink :: Maybe Boolean, smooth :: Maybe Boolean, showAllSymbol :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, stack :: Maybe String, "data" :: Maybe [ItemData] }

    type MapSeriesRec = { geoCoord :: Maybe (StrMap (Tuple Number Number)), textFixed :: Maybe (StrMap (Tuple Number Number)), nameMap :: Maybe (StrMap String), scaleLimit :: Maybe MinMax, roam :: Maybe Roam, showLegendSymbol :: Maybe Boolean, mapValuePrecision :: Maybe Number, mapValueCalculation :: Maybe MapValueCalculation, mapLocation :: Maybe Location, dataRangeHoverLink :: Maybe Boolean, hoverable :: Maybe Boolean, mapType :: Maybe String, selectedMode :: Maybe SelectedMode, "data" :: Maybe [ItemData] }

    type PieSeriesRec = { legendHoverLink :: Maybe Boolean, selectedMode :: Maybe SelectedMode, selectedOffset :: Maybe Number, roseType :: Maybe RoseType, clockWise :: Maybe Boolean, minAngle :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe Radius, center :: Maybe Center, "data" :: Maybe [ItemData] }

    type RadarSeriesRec = { legendHoverLink :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, polarIndex :: Maybe Number, "data" :: Maybe [ItemData] }

    type ScatterSeriesRec = { legendHoverLink :: Maybe Boolean, largeThreshold :: Maybe Number, large :: Maybe Boolean, symbolRotate :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, yAxisIndex :: Maybe Number, xAxisIndex :: Maybe Number, "data" :: Maybe [ItemData] }

    data Series where
      LineSeries :: { lineSeries :: LineSeriesRec, common :: UniversalSeriesRec } -> Series
      BarSeries :: { barSeries :: BarSeriesRec, common :: UniversalSeriesRec } -> Series
      ScatterSeries :: { scatterSeries :: ScatterSeriesRec, common :: UniversalSeriesRec } -> Series
      CandlestickSeries :: { candlestickSeries :: CandlestickSeriesRec, common :: UniversalSeriesRec } -> Series
      PieSeries :: { pieSeries :: PieSeriesRec, common :: UniversalSeriesRec } -> Series
      RadarSeries :: { radarSeries :: RadarSeriesRec, common :: UniversalSeriesRec } -> Series
      ChordSeries :: { chordSeries :: ChordSeriesRec, common :: UniversalSeriesRec } -> Series
      ForceSeries :: { forceSeries :: ForceSeriesRec, common :: UniversalSeriesRec } -> Series
      MapSeries :: { mapSeries :: MapSeriesRec, common :: UniversalSeriesRec } -> Series
      GaugeSeries :: { gaugeSeries :: GaugeSeriesRec, common :: UniversalSeriesRec } -> Series
      FunnelSeries :: { funnelSeries :: FunnelSeriesRec, common :: UniversalSeriesRec } -> Series
      EventRiverSeries :: { eventRiverSeries :: EventRiverSeriesRec, common :: UniversalSeriesRec } -> Series

    type UniversalSeriesRec = { markLine :: Maybe MarkLine, markPoint :: Maybe MarkPoint, itemStyle :: Maybe ItemStyle, clickable :: Maybe Boolean, tooltip :: Maybe Tooltip, name :: Maybe String }


### Type Class Instances

    instance chartTypeEncodeJson :: EncodeJson ChartType

    instance encodeSeries :: EncodeJson Series


### Values

    barSeriesDefault :: BarSeriesRec

    candlestickSeriesDefault :: CandlestickSeriesRec

    chordSeriesDefault :: ChordSeriesRec

    eventRiverSeriesDefault :: EventRiverSeriesRec

    forceSeriesDefault :: ForceSeriesRec

    funnelSeriesDefault :: FunnelSeriesRec

    gaugeSeriesDefault :: GaugeSeriesRec

    lineSeriesDefault :: LineSeriesRec

    mapSeriesDefault :: MapSeriesRec

    pieSeriesDefault :: PieSeriesRec

    radarSeriesDefault :: RadarSeriesRec

    scatterSeriesDefault :: ScatterSeriesRec

    setSeries :: forall e. [Series] -> Boolean -> EChart -> Eff e EChart

    universalSeriesDefault :: UniversalSeriesRec


## Module ECharts.Symbol

### Types

    data DoubleSymbolSize where
      DblSize :: Tuple Number Number -> DoubleSymbolSize
      DblFunc :: (ItemValue -> Tuple Number Number) -> DoubleSymbolSize

    data Symbol where
      Circle :: Symbol
      Rectangle :: Symbol
      Triangle :: Symbol
      Diamond :: Symbol
      EmptyCircle :: Symbol
      EmptyRectangle :: Symbol
      EmptyTriangle :: Symbol
      EmptyDiamond :: Symbol

    data SymbolSize where
      Size :: Number -> SymbolSize
      Func :: (ItemValue -> Number) -> SymbolSize


### Type Class Instances

    instance dblSymbolSizeEncodeJson :: EncodeJson DoubleSymbolSize

    instance encodeJsonSymbol :: EncodeJson Symbol

    instance symbolSizeEncodeJson :: EncodeJson SymbolSize


### Values


## Module ECharts.Timeline

### Types

    newtype Timeline where
      Timeline :: TimelineRec -> Timeline

    data TimelineControlPosition where
      TCPLeft :: TimelineControlPosition
      TCPRight :: TimelineControlPosition
      TCPNone :: TimelineControlPosition

    type TimelineRec = { "data" :: Maybe [String], currentIndex :: Maybe Number, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, controlStyle :: Maybe ItemStyle, checkpointStyle :: Maybe CheckpointStyle, label :: Maybe AxisLabel, lineStyle :: Maybe LineStyle, playInterval :: Maybe Number, loop :: Maybe Boolean, autoPlay :: Maybe Boolean, controlPosition :: Maybe TimelineControlPosition, padding :: Maybe (Corner Number), borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel, realtime :: Maybe Boolean, notMerge :: Maybe Boolean, "type" :: Maybe TimelineType, show :: Maybe Boolean }

    data TimelineType where
      TimelineTime :: TimelineType
      TimelineNumber :: TimelineType


### Type Class Instances

    instance timelineControlPositionEncodeJson :: EncodeJson TimelineControlPosition

    instance timelineEncodeJson :: EncodeJson Timeline

    instance timelineTypeEncodeJson :: EncodeJson TimelineType


### Values

    timelineDefault :: TimelineRec


## Module ECharts.Title

### Types

    data LinkTarget where
      Self :: LinkTarget
      Blank :: LinkTarget

    newtype Title where
      Title :: TitleRec -> Title

    type TitleRec = { subtextStyle :: Maybe TextStyle, textStyle :: Maybe TextStyle, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, textAlign :: Maybe HorizontalAlign, y :: Maybe YPos, x :: Maybe XPos, subtarget :: Maybe LinkTarget, sublink :: Maybe String, subtext :: Maybe String, target :: Maybe LinkTarget, link :: Maybe String, text :: Maybe String }


### Type Class Instances

    instance linkTargetEncodeJson :: EncodeJson LinkTarget

    instance titleEncodeJson :: EncodeJson Title


### Values

    titleDefault :: TitleRec


## Module ECharts.Toolbox

### Types

    newtype DataViewFeature where
      DataViewFeature :: DataViewFeatureRec -> DataViewFeature

    type DataViewFeatureRec = { lang :: Maybe [String], readOnly :: Maybe Boolean, title :: Maybe String, show :: Maybe Boolean }

    newtype DataZoomFeature where
      DataZoomFeature :: DataZoomFeatureRec -> DataZoomFeature

    type DataZoomFeatureRec = { title :: Maybe DataZoomFeatureTitle, show :: Maybe Boolean }

    newtype DataZoomFeatureTitle where
      DataZoomFeatureTitle :: DataZoomFeatureTitleRec -> DataZoomFeatureTitle

    type DataZoomFeatureTitleRec = { dataZoomReset :: String, dataZoom :: String }

    newtype Feature where
      Feature :: FeatureRec -> Feature

    type FeatureRec = { saveAsImage :: Maybe SaveAsImageFeature, restore :: Maybe RestoreFeature, magicType :: Maybe MagicTypeFeature, dataView :: Maybe DataViewFeature, dataZoom :: Maybe DataZoomFeature, mark :: Maybe MarkFeature }

    data MagicType where
      MagicLine :: MagicType
      MagicBar :: MagicType
      MagicStack :: MagicType
      MagicTiled :: MagicType
      MagicForce :: MagicType
      MagicChord :: MagicType
      MagicPie :: MagicType
      MagicFunnel :: MagicType

    newtype MagicTypeFeature where
      MagicTypeFeature :: MagicTypeFeatureRec -> MagicTypeFeature

    type MagicTypeFeatureRec = { "type" :: Maybe [MagicType], option :: Maybe Json, title :: Maybe (StrMap String), show :: Maybe Boolean }

    newtype MarkFeature where
      MarkFeature :: MarkFeatureRec -> MarkFeature

    type MarkFeatureRec = { lineStyle :: Maybe LineStyle, title :: Maybe MarkFeatureTitle, show :: Maybe Boolean }

    newtype MarkFeatureTitle where
      MarkFeatureTitle :: MarkFeatureTitleRec -> MarkFeatureTitle

    type MarkFeatureTitleRec = { markClear :: String, markUndo :: String, mark :: Maybe String }

    newtype RestoreFeature where
      RestoreFeature :: RestoreFeatureRec -> RestoreFeature

    type RestoreFeatureRec = { title :: Maybe String, show :: Maybe Boolean }

    newtype SaveAsImageFeature where
      SaveAsImageFeature :: SaveAsImageFeatureRec -> SaveAsImageFeature

    type SaveAsImageFeatureRec = { lang :: Maybe [String], "type" :: Maybe ImgType, title :: Maybe String, show :: Maybe Boolean }

    newtype Toolbox where
      Toolbox :: ToolboxRec -> Toolbox

    type ToolboxRec = { feature :: Maybe Feature, textStyle :: Maybe TextStyle, showTitle :: Maybe Boolean, effectiveColor :: Maybe Color, disableColor :: Maybe Color, color :: Maybe [Color], itemSize :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean }


### Type Class Instances

    instance dataViewFeatureEncodeJson :: EncodeJson DataViewFeature

    instance dataviewFeatureEncodeJson :: EncodeJson DataZoomFeature

    instance datazoomTitleEncodeJson :: EncodeJson DataZoomFeatureTitle

    instance featureEncodeJson :: EncodeJson Feature

    instance magicTypeEncodeJson :: EncodeJson MagicType

    instance magicTypeFeatureEncodeJson :: EncodeJson MagicTypeFeature

    instance markFeatureEncodeJson :: EncodeJson MarkFeature

    instance mftitleEncodeJson :: EncodeJson MarkFeatureTitle

    instance restoreFeatureEncodeJson :: EncodeJson RestoreFeature

    instance saveAsImageEncodeJson :: EncodeJson SaveAsImageFeature

    instance toolboxEncodeJson :: EncodeJson Toolbox


### Values

    dataViewFeatureDefault :: DataViewFeatureRec

    dataZoomFeatureDefault :: DataZoomFeatureRec

    featureDefault :: FeatureRec

    magicTypeFeatureDefault :: MagicTypeFeatureRec

    markFeatureDefault :: MarkFeatureRec

    restoreFeatureDefault :: RestoreFeatureRec

    saveAsImageFeatureDefault :: SaveAsImageFeatureRec

    toolboxDefault :: ToolboxRec


## Module ECharts.Tooltip

### Types

    newtype Tooltip where
      Tooltip :: TooltipRec -> Tooltip

    newtype TooltipAxisPointer where
      TooltipAxisPointer :: TooltipAxisPointerRec -> TooltipAxisPointer

    type TooltipAxisPointerRec = { shadowStyle :: Maybe AreaStyle, crossStyle :: Maybe LineStyle, lineStyle :: Maybe LineStyle, "type" :: Maybe TooltipAxisPointerType }

    data TooltipAxisPointerType where
      LinePointer :: TooltipAxisPointerType
      CrossPointer :: TooltipAxisPointerType
      ShadowPointer :: TooltipAxisPointerType
      NonePointer :: TooltipAxisPointerType

    data TooltipPosition where
      Fixed :: [Number] -> TooltipPosition
      FuncPos :: ([Number] -> [Number]) -> TooltipPosition

    type TooltipRec = { enterable :: Maybe Boolean, textStyle :: Maybe TextStyle, axisPointer :: Maybe TooltipAxisPointer, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderRadius :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, transitionDuration :: Maybe Number, hideDelay :: Maybe Number, showDelay :: Maybe Number, islandFormatter :: Maybe Formatter, formatter :: Maybe Formatter, position :: Maybe TooltipPosition, trigger :: Maybe TooltipTrigger, showContent :: Maybe Boolean, show :: Maybe Boolean }

    data TooltipTrigger where
      TriggerItem :: TooltipTrigger
      TriggerAxis :: TooltipTrigger


### Type Class Instances

    instance tooltipAxisPointerEncodeJson :: EncodeJson TooltipAxisPointer

    instance tooltipAxisPointerTypeEncodeJson :: EncodeJson TooltipAxisPointerType

    instance tooltipEncodeJson :: EncodeJson Tooltip

    instance tooltipPositionEncodeJson :: EncodeJson TooltipPosition

    instance tooltipTriggerEncodeJson :: EncodeJson TooltipTrigger


### Values

    tooltipAxisPointerDefault :: TooltipAxisPointerRec

    tooltipDefault :: TooltipRec


## Module ECharts.Utils

### Values

    unnull :: Json -> Json


## Module ECharts.Item.Data

### Types

    data ItemData where
      Value :: ItemValue -> ItemData
      Dat :: ItemDataDatRec -> ItemData
      Label :: String -> ItemData

    type ItemDataDatRec = { selected :: Maybe Boolean, itemStyle :: Maybe ItemStyle, tooltip :: Maybe Tooltip, name :: Maybe String, value :: ItemValue }


### Type Class Instances

    instance itemDataEncodeJson :: EncodeJson ItemData


### Values

    dataDefault :: ItemValue -> ItemDataDatRec


## Module ECharts.Item.Value

### Types

    type HLOCRec = { c :: Number, o :: Number, l :: Number, h :: Number }

    data ItemValue where
      None :: ItemValue
      Simple :: Number -> ItemValue
      Many :: [Number] -> ItemValue
      XYR :: XYRRec -> ItemValue
      HLOC :: HLOCRec -> ItemValue

    type XYRRec = { r :: Maybe Number, y :: Number, x :: Number }


### Type Class Instances

    instance itemValueEncodeJson :: EncodeJson ItemValue


## Module ECharts.Mark.Data

### Types

    newtype MarkPointData where
      MarkPointData :: MarkPointDataRec -> MarkPointData

    type MarkPointDataRec = { "type" :: Maybe String, yAxis :: Maybe Number, xAxis :: Maybe Number, y :: Maybe Number, x :: Maybe Number, value :: Maybe Number, name :: Maybe String }


### Type Class Instances

    instance mpDataEncodeJson :: EncodeJson MarkPointData


### Values

    markPointDataDefault :: MarkPointDataRec


## Module ECharts.Mark.Effect

### Types

    newtype MarkPointEffect where
      MarkPointEffect :: MarkPointEffectRec -> MarkPointEffect

    type MarkPointEffectRec = { shadowBlur :: Maybe Number, color :: Maybe Color, scaleSize :: Maybe Boolean, period :: Maybe Boolean, loop :: Maybe Boolean, show :: Maybe Boolean }


### Type Class Instances

    instance mpEffectEncodeJson :: EncodeJson MarkPointEffect


### Values

    markPointEffectDefault :: MarkPointEffectRec


## Module ECharts.Mark.Line

### Types

    newtype MarkLine where
      MarkLine :: MarkLineRec -> MarkLine

    type MarkLineRec = { itemStyle :: Maybe ItemStyle, "data" :: Maybe [Tuple MarkPointData MarkPointData], geoCoord :: Maybe [GeoCoord], effect :: Maybe MarkPointEffect, symbolRotate :: Maybe (Tuple Number Number), symbolSize :: Maybe DoubleSymbolSize, symbol :: Maybe (Tuple Symbol Symbol) }


### Type Class Instances

    instance mlEncodeJson :: EncodeJson MarkLine


### Values

    addMarkLine :: forall e a. MarkLine -> EChart -> Eff (addMarkLineECharts :: AddMarkLine | e) EChart

    delMarkLine :: forall e. Number -> String -> EChart -> Eff (removeMarkLine :: RemoveMarkLine | e) EChart

    markLineDefault :: MarkLineRec


## Module ECharts.Mark.Point

### Types

    newtype MarkPoint where
      MarkPoint :: MarkPointRec -> MarkPoint

    type MarkPointRec = { geoCoord :: Maybe (StrMap (Tuple Number Number)), "data" :: Maybe [MarkPointData], effect :: Maybe MarkPointEffect, large :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol }


### Type Class Instances

    instance markPointEncodeJson :: EncodeJson MarkPoint


### Values

    addMarkPoint :: forall e. MarkPoint -> EChart -> Eff (addMarkPointECharts :: AddMarkPoint | e) EChart

    delMarkPoint :: forall e. Number -> String -> EChart -> Eff (removeMarkPointECharts :: RemoveMarkPoint | e) EChart

    markPointDefault :: MarkPointRec


## Module ECharts.Series.EventRiver

### Types

    newtype Evolution where
      Evolution :: EvolutionRec -> Evolution

    newtype EvolutionDetail where
      EvolutionDetail :: EvolutionDetailRec -> EvolutionDetail

    type EvolutionDetailRec = { img :: Maybe String, text :: Maybe String, link :: Maybe String }

    type EvolutionRec = { detail :: Maybe EvolutionDetail, value :: Number, time :: Date }

    newtype OneEvent where
      OneEvent :: OneEventRec -> OneEvent

    type OneEventRec = { evolution :: Maybe [Evolution], weight :: Maybe Number, name :: Maybe String }


### Type Class Instances

    instance evoDetailEncodeJson :: EncodeJson EvolutionDetail

    instance evoEncodeJson :: EncodeJson Evolution

    instance oneEventEncodeJson :: EncodeJson OneEvent


### Values

    evolutionDetailDefault :: EvolutionDetailRec

    oneEventDefault :: OneEventRec


## Module ECharts.Series.Force

### Types

    newtype ForceCategory where
      ForceCategory :: ForceCategoryRec -> ForceCategory

    type ForceCategoryRec = { itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, name :: Maybe String }

    newtype Link where
      Link :: LinkRec -> Link

    data LinkEnd where
      Name :: String -> LinkEnd
      Index :: Number -> LinkEnd

    type LinkRec = { itemStyle :: Maybe ItemStyle, weight :: Number, target :: LinkEnd, source :: LinkEnd }

    type Matrix = [[Number]]

    newtype Node where
      Node :: NodeRec -> Node

    type NodeRec = { category :: Maybe Number, draggable :: Maybe Boolean, fixY :: Maybe Boolean, fixX :: Maybe Boolean, initial :: Maybe (Tuple Number Number), itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, ignore :: Maybe Boolean, value :: Number, label :: Maybe String, name :: Maybe String }


### Type Class Instances

    instance forceCategoryEncodeJson :: EncodeJson ForceCategory

    instance linkEncodeJson :: EncodeJson Link

    instance linkEndEncodeJson :: EncodeJson LinkEnd

    instance nodeEncodeJson :: EncodeJson Node


### Values

    forceCategoryDefault :: ForceCategoryRec

    nodeDefault :: Number -> NodeRec


## Module ECharts.Series.Gauge

### Types

    newtype GaugeDetail where
      GaugeDetail :: GaugeDetailRec -> GaugeDetail

    type GaugeDetailRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, offsetCenter :: Maybe (Tuple PercentOrPixel PercentOrPixel), height :: Maybe Number, width :: Maybe Number, borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, show :: Maybe Boolean }

    newtype Pointer where
      Pointer :: PointerRec -> Pointer

    type PointerRec = { color :: Maybe Color, width :: Maybe Number, length :: Maybe Number }

    newtype SplitLine where
      SplitLine :: SplitLineRec -> SplitLine

    type SplitLineRec = { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean }


### Type Class Instances

    instance gaugeDetailEncodeJson :: EncodeJson GaugeDetail

    instance pointerEncodeJson :: EncodeJson Pointer

    instance splitLineEncodeJson :: EncodeJson SplitLine


### Values

    gaugeDetailDefault :: GaugeDetailRec

    pointerDefault :: PointerRec

    splitLineDefault :: SplitLineRec


## Module ECharts.Style.Area

### Types

    newtype AreaStyle where
      AreaStyle :: Color -> AreaStyle


### Type Class Instances

    instance areaStyleEncodeJson :: EncodeJson AreaStyle


## Module ECharts.Style.Checkpoint

### Types

    newtype CheckpointStyle where
      CheckpointStyle :: CheckpointStyleRec -> CheckpointStyle

    type CheckpointStyleRec = { label :: Maybe AxisLabel, borderColor :: Maybe Color, color :: Maybe Color, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol }


### Type Class Instances

    instance checkpointStyleEncodeJson :: EncodeJson CheckpointStyle


### Values

    checkpointStyleDefault :: CheckpointStyleRec


## Module ECharts.Style.Chord

### Types

    newtype ChordStyle where
      ChordStyle :: ChordStyleRec -> ChordStyle

    type ChordStyleRec = { borderColor :: Maybe Color, borderWidth :: Maybe Number, color :: Maybe Color, width :: Maybe Number }


### Type Class Instances

    instance chordStyleJson :: EncodeJson ChordStyle


### Values

    chordStyleDefault :: ChordStyleRec


## Module ECharts.Style.Item

### Types

    newtype IStyle where
      IStyle :: IStyleRec -> IStyle

    type IStyleRec = { linkStyle :: Maybe LinkStyle, nodeStyle :: Maybe NodeStyle, chordStyle :: Maybe ChordStyle, areaStyle :: Maybe AreaStyle, lineStyle :: Maybe LineStyle, labelLine :: Maybe ItemLabelLine, label :: Maybe ItemLabel, barBorderWidth :: Maybe Number, barBorderRadius :: Maybe (Corner Number), barBorderColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe CalculableColor }

    newtype ItemLabel where
      ItemLabel :: ItemLabelRec -> ItemLabel

    newtype ItemLabelLine where
      ItemLabelLine :: ItemLabelLineRec -> ItemLabelLine

    type ItemLabelLineRec = { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean }

    type ItemLabelRec = { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, distance :: Maybe Boolean, rotate :: Maybe Boolean, position :: Maybe LabelPosition, show :: Maybe Boolean }

    newtype ItemStyle where
      ItemStyle :: ItemStyleRec -> ItemStyle

    type ItemStyleRec = { emphasis :: Maybe IStyle, normal :: Maybe IStyle }


### Type Class Instances

    instance istyleEncodeJson :: EncodeJson IStyle

    instance itemLabelEncodeJson :: EncodeJson ItemLabel

    instance itemLabelLineEncodeJson :: EncodeJson ItemLabelLine

    instance itemStyleEncodeJson :: EncodeJson ItemStyle


### Values

    istyleDefault :: IStyleRec

    itemLabelDefault :: ItemLabelRec

    itemLabelLineDefault :: ItemLabelLineRec

    itemStyleDefault :: ItemStyleRec


## Module ECharts.Style.Line

### Types

    newtype LineStyle where
      LineStyle :: LineStyleRec -> LineStyle

    type LineStyleRec = { shadowOffsetY :: Maybe Number, shadowOffsetX :: Maybe Number, shadowColor :: Maybe Color, width :: Maybe Number, "type" :: Maybe LineType, color :: Maybe Color }

    data LineType where
      Solid :: LineType
      Dotted :: LineType
      Dashed :: LineType


### Type Class Instances

    instance lineStyleEncodeJson :: EncodeJson LineStyle

    instance linetypeEncodeJson :: EncodeJson LineType


### Values

    lineStyleDefault :: LineStyleRec


## Module ECharts.Style.Link

### Types

    newtype LinkStyle where
      LinkStyle :: LinkStyleRec -> LinkStyle

    type LinkStyleRec = { width :: Maybe Number, color :: Maybe Color, "type" :: Maybe LinkType }

    data LinkType where
      LTCurve :: LinkType
      LTLine :: LinkType


### Type Class Instances

    instance linkStyleEncodeJson :: EncodeJson LinkStyle

    instance linkTypeEncodeJson :: EncodeJson LinkType


### Values

    linkStyleDefault :: LinkStyleRec


## Module ECharts.Style.Node

### Types

    newtype NodeStyle where
      NodeStyle :: NodeStyleRec -> NodeStyle

    type NodeStyleRec = { borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe Color }


### Type Class Instances

    instance nodeStyleEncodeJson :: EncodeJson NodeStyle


### Values

    nodeStyleDefault :: NodeStyleRec


## Module ECharts.Style.Text

### Types

    type Decoration = String

    type FontFamily = String

    data FontStyle where
      FSNormal :: FontStyle
      FSItalic :: FontStyle
      FSOblique :: FontStyle

    data FontWeight where
      FWNormal :: FontWeight
      FWBold :: FontWeight
      FWBolder :: FontWeight
      FWLighter :: FontWeight
      FW100 :: FontWeight
      FW200 :: FontWeight
      FW300 :: FontWeight
      FW400 :: FontWeight
      FW500 :: FontWeight
      FW600 :: FontWeight
      FW700 :: FontWeight
      FW800 :: FontWeight
      FW900 :: FontWeight

    data TextBaseline where
      TBLTop :: TextBaseline
      TBLBottom :: TextBaseline
      TBLMiddle :: TextBaseline

    newtype TextStyle where
      TextStyle :: TextStyleRec -> TextStyle

    type TextStyleRec = { fontWeight :: Maybe FontWeight, fontStyle :: Maybe FontStyle, fontSize :: Maybe Number, fontFamily :: Maybe FontFamily, baseline :: Maybe TextBaseline, align :: Maybe HorizontalAlign, decoration :: Maybe Decoration, color :: Maybe Color }


### Type Class Instances

    instance fontStyleEncodeJson :: EncodeJson FontStyle

    instance fontWeightEncodeJson :: EncodeJson FontWeight

    instance textBaselineEncodeJson :: EncodeJson TextBaseline

    instance textStyleEncodeJson :: EncodeJson TextStyle


### Values

    textStyleDefault :: TextStyleRec



