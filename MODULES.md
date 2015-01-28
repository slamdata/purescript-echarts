# Module Documentation

## Module ECharts.AddData

### Types

    newtype AdditionalData where
      AdditionalData :: { additionalData :: Maybe String, dataGrow :: Boolean, isHead :: Boolean, datum :: ItemData, idx :: Number } -> AdditionalData


### Type Class Instances

    instance additionalDataEncodeJson :: EncodeJson AdditionalData


### Values

    addData :: forall e. AdditionalData -> EChart -> Eff (dataAdd :: AddData | e) EChart


## Module ECharts.Axis

### Types

    newtype Axis where
      Axis :: { "data" :: Maybe [AxisData], splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisTick :: Maybe AxisTick, axisLine :: Maybe AxisLine, splitNumber :: Maybe Number, scale :: Maybe Boolean, max :: Maybe Number, min :: Maybe Number, boundaryGap :: Maybe AxisBoundaryGap, nameTextStyle :: Maybe TextStyle, nameLocation :: Maybe AxisNameLocation, name :: Maybe String, position :: Maybe AxisPosition, show :: Maybe Boolean, "type" :: Maybe AxisType } -> Axis

    data AxisBoundaryGap where
      CatBoundaryGap :: Boolean -> AxisBoundaryGap
      ValueBoundaryGap :: Number -> Number -> AxisBoundaryGap

    data AxisData where
      CommonAxisData :: String -> AxisData
      CustomAxisData :: { textStyle :: TextStyle, value :: String } -> AxisData

    newtype AxisLabel where
      AxisLabel :: { clickable :: Maybe Boolean, margin :: Maybe Number, rotate :: Maybe Number, textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, interval :: Maybe Interval, show :: Maybe Boolean } -> AxisLabel

    newtype AxisLine where
      AxisLine :: { lineStyle :: Maybe AxisLineStyle, onZero :: Maybe Boolean, show :: Maybe Boolean } -> AxisLine

    newtype AxisLineStyle where
      AxisLineStyle :: { width :: Maybe Number, color :: Maybe [Tuple Number Color] } -> AxisLineStyle

    data AxisNameLocation where
      Start :: AxisNameLocation
      End :: AxisNameLocation

    data AxisPosition where
      LeftAxis :: AxisPosition
      RightAxis :: AxisPosition
      TopAxis :: AxisPosition
      BottomAxis :: AxisPosition

    newtype AxisSplitArea where
      AxisSplitArea :: { areaStyle :: Maybe AreaStyle, onGap :: Maybe Boolean, show :: Maybe Boolean } -> AxisSplitArea

    newtype AxisSplitLine where
      AxisSplitLine :: { lineStyle :: Maybe LineStyle, onGap :: Maybe Boolean, show :: Maybe Boolean } -> AxisSplitLine

    newtype AxisTick where
      AxisTick :: { inside :: Maybe Boolean, onGap :: Maybe Boolean, interval :: Maybe Interval, lineStyle :: Maybe LineStyle, length :: Maybe Number, splitNumber :: Maybe Number, show :: Maybe Boolean } -> AxisTick

    data AxisType where
      CategoryAxis :: AxisType
      ValueAxis :: AxisType
      TimeAxis :: AxisType

    data Axises where
      OneAxis :: Axis -> Axises
      TwoAxises :: Axis -> Axis -> Axises

    newtype Indicator where
      Indicator :: { axisLabel :: Maybe AxisLabel, max :: Maybe Number, min :: Maybe Number, text :: Maybe String } -> Indicator

    newtype Polar where
      Polar :: { indicator :: Maybe [Indicator], "type" :: Maybe PolarType, splitArea :: Maybe AxisSplitArea, splitLine :: Maybe AxisSplitLine, axisLabel :: Maybe AxisLabel, axisLine :: Maybe AxisLine, scale :: Maybe Boolean, boundaryGap :: Maybe (Tuple Number Number), name :: Maybe PolarName, splitNumber :: Maybe Number, startAngle :: Maybe Number, radius :: Maybe PercentOrPixel, center :: Maybe (Tuple PercentOrPixel PercentOrPixel) } -> Polar

    newtype PolarName where
      PolarName :: { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, show :: Maybe Boolean } -> PolarName

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

    init :: forall e. Maybe Theme -> Node -> Eff (echartInit :: EChartInit, dom :: DOM | e) EChart

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
      ColorFuncParam :: { "data" :: { name :: String, value :: ItemValue }, dataIndex :: Number, series :: String, seriesIndex :: Number } -> ColorFuncParam


### Type Class Instances

    instance calculableColorEncodeJson :: EncodeJson CalculableColor


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
      MinMax :: { max :: Number, min :: Number } -> MinMax

    data PercentOrPixel where
      Percent :: Number -> PercentOrPixel
      Pixel :: Number -> PercentOrPixel

    data Radius where
      R :: PercentOrPixel -> Radius
      Rs :: { outer :: PercentOrPixel, inner :: PercentOrPixel } -> Radius

    data Roam where
      Enable :: Roam
      Disable :: Roam
      Scale :: Roam
      Move :: Roam

    data RoseType where
      RTRadius :: RoseType
      RTArea :: RoseType

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
      Location :: { y :: Maybe YPos, x :: Maybe XPos } -> Location

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
      DataRange :: { textStyle :: Maybe TextStyle, text :: Maybe (Tuple String String), formatter :: Maybe Formatter, color :: Maybe [Color], realtime :: Maybe Boolean, hoverLink :: Maybe Boolean, calculable :: Maybe Boolean, selectedMode :: Maybe SelectedMode, splitNumber :: Maybe Number, precision :: Maybe Number, max :: Maybe Number, min :: Maybe Number, itemHeight :: Maybe Number, itemWidth :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean } -> DataRange


### Type Class Instances

    instance dataRangeEncodeJson :: EncodeJson DataRange


## Module ECharts.DataZoom

### Types

    newtype DataZoom where
      DataZoom :: { zoomlock :: Maybe Boolean, realtime :: Maybe Boolean, showDetail :: Maybe Boolean, end :: Maybe Number, start :: Maybe Number, yAxisIndex :: Maybe [Number], xAxisIndex :: Maybe [Number], handleColor :: Maybe Color, fillerColor :: Maybe Color, dataBackgroundColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean } -> DataZoom


### Type Class Instances

    instance dataZoomEncodeJson :: EncodeJson DataZoom


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


### Type Class Instances

    instance eventTypeShow :: Show EventType


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
      Grid :: { borderColor :: Maybe Number, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel } -> Grid


### Type Class Instances

    instance gridEncodeJson :: EncodeJson Grid


## Module ECharts.Image

### Types

    data ImgType where
      PNG :: ImgType
      JPEG :: ImgType


### Type Class Instances

    instance encodeImg :: EncodeJson ImgType

    instance imgTypeShow :: Show ImgType


### Values

    getDataURL :: forall e. ImgType -> EChart -> Eff (image :: ImageMaking | e) String

    getImage :: forall e. ImgType -> EChart -> Eff (image :: ImageMaking, dom :: DOM | e) Node


## Module ECharts.Legend

### Types

    newtype Legend where
      Legend :: { "data" :: Maybe [LegendItem], selected :: Maybe (StrMap Boolean), selectedMode :: Maybe SelectedMode, formatter :: Maybe Formatter, textStyle :: Maybe TextStyle, itemWidth :: Maybe Number, itemHeight :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean } -> Legend

    data LegendItem where
      LegendItem :: String -> { textStyle :: Maybe TextStyle, icon :: Maybe String } -> LegendItem


### Type Class Instances

    instance legendEncodeJson :: EncodeJson Legend

    instance legendItemEncodeJson :: EncodeJson LegendItem


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
      LoadingOption :: { progress :: Maybe Number, effectOption :: Maybe Json, effect :: Maybe LoadingEffect, textStyle :: Maybe TextStyle, y :: Maybe YPos, x :: Maybe XPos, text :: Maybe String } -> LoadingOption


### Type Class Instances

    instance loadingEffectEncodeJson :: EncodeJson LoadingEffect

    instance showLoadingOptions :: EncodeJson LoadingOption


### Values

    hideLoading :: forall e. EChart -> Eff (hideLoadingECharts :: LoadingHide | e) EChart

    showLoading :: forall e. LoadingOption -> EChart -> Eff (showLoadingECharts :: LoadingShow | e) EChart


## Module ECharts.Options

### Types

    newtype Option where
      Option :: { series :: Maybe [Maybe Series], polar :: Maybe [Polar], yAxis :: Maybe Axises, xAxis :: Maybe Axises, grid :: Maybe Grid, roamController :: Maybe RoamController, dataZoom :: Maybe DataZoom, dataRange :: Maybe DataRange, legend :: Maybe Legend, title :: Maybe Title, toolbox :: Maybe Toolbox, tooltip :: Maybe Tooltip, timeline :: Maybe Timeline, animation :: Maybe Boolean, calculable :: Maybe Boolean, renderAsImage :: Maybe Boolean, color :: Maybe [Color], backgroundColor :: Maybe Color } -> Option


### Type Class Instances

    instance optionsEncodeJson :: EncodeJson Option


### Values

    setOption :: forall e. Option -> Boolean -> EChart -> Eff (echartSetOption :: EChartOptionSet | e) EChart


## Module ECharts.RoamController

### Types

    newtype RoamController where
      RoamController :: { mapTypeControl :: StrMap Boolean, step :: Maybe Number, handleColor :: Maybe Color, fillerColor :: Maybe Color, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, height :: Maybe Number, width :: Maybe Number, y :: Maybe YPos, x :: Maybe XPos, show :: Maybe Boolean } -> RoamController


### Type Class Instances

    instance roamControllerEncodeJson :: EncodeJson RoamController


## Module ECharts.Series

### Types

    data Series where
      LineSeries :: { special :: LineSeriesRec, common :: UniversalSeriesRec } -> Series
      BarSeries :: { special :: BarSeriesRec, common :: UniversalSeriesRec } -> Series
      ScatterSeries :: { special :: ScatterSeriesRec, common :: UniversalSeriesRec } -> Series
      CandlestickSeries :: { special :: CandlestickSeriesRec, common :: UniversalSeriesRec } -> Series
      PieSeries :: { special :: PieSeriesRec, common :: UniversalSeriesRec } -> Series
      RadarSeries :: { special :: RadarSeriesRec, common :: UniversalSeriesRec } -> Series
      ChordSeries :: { special :: ChordSeriesRec, common :: UniversalSeriesRec } -> Series
      ForceSeries :: { special :: ForceSeriesRec, common :: UniversalSeriesRec } -> Series
      MapSeries :: { special :: MapSeriesRec, common :: UniversalSeriesRec } -> Series
      GaugeSeries :: { special :: GaugeSeriesRec, common :: UniversalSeriesRec } -> Series
      FunnelSeries :: { special :: FunnelSeriesRec, common :: UniversalSeriesRec } -> Series
      EventRiverSeries :: { special :: EventRiverSeriesRec, common :: UniversalSeriesRec } -> Series


### Type Class Instances

    instance encodeSeries :: EncodeJson Series


### Values

    setSeries :: forall e. [Series] -> Boolean -> EChart -> Eff e EChart


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


## Module ECharts.Timeline

### Types

    newtype Timeline where
      Timeline :: { "data" :: Maybe [String], currentIndex :: Maybe Number, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, controlStyle :: Maybe ItemStyle, checkpointStyle :: Maybe CheckpointStyle, label :: Maybe AxisLabel, lineStyle :: Maybe LineStyle, playInterval :: Maybe Number, loop :: Maybe Boolean, autoPlay :: Maybe Boolean, controlPosition :: Maybe TimelineControlPosition, padding :: Maybe (Corner Number), borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, height :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, x :: Maybe PercentOrPixel, realtime :: Maybe Boolean, notMerge :: Maybe Boolean, "type" :: Maybe TimelineType, show :: Maybe Boolean } -> Timeline

    data TimelineControlPosition where
      TCPLeft :: TimelineControlPosition
      TCPRight :: TimelineControlPosition
      TCPNone :: TimelineControlPosition

    data TimelineType where
      TimelineTime :: TimelineType
      TimelineNumber :: TimelineType


### Type Class Instances

    instance timelineControlPositionEncodeJson :: EncodeJson TimelineControlPosition

    instance timelineEncodeJson :: EncodeJson Timeline

    instance timelineTypeEncodeJson :: EncodeJson TimelineType


## Module ECharts.Title

### Types

    data LinkTarget where
      Self :: LinkTarget
      Blank :: LinkTarget

    newtype Title where
      Title :: { subtextStyle :: Maybe TextStyle, textStyle :: Maybe TextStyle, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, textAlign :: Maybe HorizontalAlign, y :: Maybe YPos, x :: Maybe XPos, subtarget :: Maybe LinkTarget, sublink :: Maybe String, subtext :: Maybe String, target :: Maybe LinkTarget, link :: Maybe String, text :: Maybe String } -> Title


### Type Class Instances

    instance linkTargetEncodeJson :: EncodeJson LinkTarget

    instance titleEncodeJson :: EncodeJson Title


## Module ECharts.Toolbox

### Types

    newtype DataViewFeature where
      DataViewFeature :: { lang :: Maybe [String], readOnly :: Maybe Boolean, title :: Maybe String, show :: Maybe Boolean } -> DataViewFeature

    newtype DataZoomFeature where
      DataZoomFeature :: { title :: Maybe DataZoomFeatureTitle, show :: Maybe Boolean } -> DataZoomFeature

    newtype DataZoomFeatureTitle where
      DataZoomFeatureTitle :: { dataZoomReset :: String, dataZoom :: String } -> DataZoomFeatureTitle

    newtype Feature where
      Feature :: { saveAsImage :: Maybe SaveAsImageFeature, restore :: Maybe RestoreFeature, magicType :: Maybe MagicTypeFeature, dataView :: Maybe DataViewFeature, dataZoom :: Maybe DataZoomFeature, mark :: Maybe MarkFeature } -> Feature

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
      MagicTypeFeature :: { "type" :: Maybe [MagicType], option :: Maybe Json, title :: Maybe (StrMap String), show :: Maybe Boolean } -> MagicTypeFeature

    newtype MarkFeature where
      MarkFeature :: { lineStyle :: Maybe LineStyle, title :: Maybe MarkFeatureTitle, show :: Maybe Boolean } -> MarkFeature

    newtype MarkFeatureTitle where
      MarkFeatureTitle :: { markClear :: String, markUndo :: String, mark :: Maybe String } -> MarkFeatureTitle

    newtype RestoreFeature where
      RestoreFeature :: { title :: Maybe String, show :: Maybe Boolean } -> RestoreFeature

    newtype SaveAsImageFeature where
      SaveAsImageFeature :: { lang :: Maybe [String], "type" :: Maybe ImgType, title :: Maybe String, show :: Maybe Boolean } -> SaveAsImageFeature

    newtype Toolbox where
      Toolbox :: { feature :: Maybe Feature, textStyle :: Maybe TextStyle, showTitle :: Maybe Boolean, effectiveColor :: Maybe Color, disableColor :: Maybe Color, color :: Maybe [Color], itemSize :: Maybe Number, itemGap :: Maybe Number, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, y :: Maybe YPos, x :: Maybe XPos, orient :: Maybe Orient, show :: Maybe Boolean } -> Toolbox


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


## Module ECharts.Tooltip

### Types

    newtype Tooltip where
      Tooltip :: { enterable :: Maybe Boolean, textStyle :: Maybe TextStyle, axisPointer :: Maybe TooltipAxisPointer, padding :: Maybe (Corner Number), borderWidth :: Maybe Number, borderRadius :: Maybe Number, borderColor :: Maybe Color, backgroundColor :: Maybe Color, transitionDuration :: Maybe Number, hideDelay :: Maybe Number, showDelay :: Maybe Number, islandFormatter :: Maybe Formatter, formatter :: Maybe Formatter, position :: Maybe TooltipPosition, trigger :: Maybe TooltipTrigger, showContent :: Maybe Boolean, show :: Maybe Boolean } -> Tooltip

    newtype TooltipAxisPointer where
      TooltipAxisPointer :: { shadowStyle :: Maybe AreaStyle, crossStyle :: Maybe LineStyle, lineStyle :: Maybe LineStyle, "type" :: Maybe TooltipAxisPointerType } -> TooltipAxisPointer

    data TooltipAxisPointerType where
      LinePointer :: TooltipAxisPointerType
      CrossPointer :: TooltipAxisPointerType
      ShadowPointer :: TooltipAxisPointerType
      NonePointer :: TooltipAxisPointerType

    data TooltipPosition where
      Fixed :: [Number] -> TooltipPosition
      FuncPos :: ([Number] -> [Number]) -> TooltipPosition

    data TooltipTrigger where
      TriggerItem :: TooltipTrigger
      TriggerAxis :: TooltipTrigger


### Type Class Instances

    instance tooltipAxisPointerEncodeJson :: EncodeJson TooltipAxisPointer

    instance tooltipAxisPointerTypeEncodeJson :: EncodeJson TooltipAxisPointerType

    instance tooltipEncodeJson :: EncodeJson Tooltip

    instance tooltipPositionEncodeJson :: EncodeJson TooltipPosition

    instance tooltipTriggerEncodeJson :: EncodeJson TooltipTrigger


## Module ECharts.Type

### Types

    data ChartType where
      Line :: ChartType
      Bar :: ChartType
      Scatter :: ChartType
      Candlestick :: ChartType
      Pie :: ChartType
      Radar :: ChartType
      Chord :: ChartType
      Force :: ChartType
      Map :: ChartType
      Gauge :: ChartType
      Funnel :: ChartType
      EventRiver :: ChartType


### Type Class Instances

    instance chartTypeEncodeJson :: EncodeJson ChartType


## Module ECharts.Utils

### Values

    func2json :: forall a. a -> Json

    unnull :: Json -> Json


## Module ECharts.Item.Data

### Types

    data ItemData where
      Value :: ItemValue -> ItemData
      Dat :: { selected :: Maybe Boolean, itemStyle :: Maybe ItemStyle, tooltip :: Maybe Tooltip, name :: Maybe String, value :: ItemValue } -> ItemData
      Label :: String -> ItemData


### Type Class Instances

    instance itemDataEncodeJson :: EncodeJson ItemData


### Values

    dataDefault :: ItemValue -> _


## Module ECharts.Item.Value

### Types

    data ItemValue where
      None :: ItemValue
      Simple :: Number -> ItemValue
      Many :: [Number] -> ItemValue
      XYR :: { r :: Maybe Number, y :: Number, x :: Number } -> ItemValue
      HLOC :: { c :: Number, o :: Number, l :: Number, h :: Number } -> ItemValue


### Type Class Instances

    instance itemValueEncodeJson :: EncodeJson ItemValue


## Module ECharts.Mark.Data

### Types

    newtype MarkPointData where
      MarkPointData :: { "type" :: Maybe String, yAxis :: Maybe Number, xAxis :: Maybe Number, y :: Maybe Number, x :: Maybe Number, value :: Maybe Number, name :: Maybe String } -> MarkPointData


### Type Class Instances

    instance mpDataEncodeJson :: EncodeJson MarkPointData


## Module ECharts.Mark.Effect

### Types

    newtype MarkPointEffect where
      MarkPointEffect :: { shadowBlur :: Maybe Number, color :: Maybe Color, scaleSize :: Maybe Boolean, period :: Maybe Boolean, loop :: Maybe Boolean, show :: Maybe Boolean } -> MarkPointEffect


### Type Class Instances

    instance mpEffectEncodeJson :: EncodeJson MarkPointEffect


## Module ECharts.Mark.Line

### Types

    newtype MarkLine where
      MarkLine :: { itemStyle :: Maybe ItemStyle, "data" :: Maybe [Tuple MarkPointData MarkPointData], geoCoord :: Maybe [GeoCoord], effect :: Maybe MarkPointEffect, symbolRotate :: Maybe (Tuple Number Number), symbolSize :: Maybe DoubleSymbolSize, symbol :: Maybe (Tuple Symbol Symbol) } -> MarkLine


### Type Class Instances

    instance mlEncodeJson :: EncodeJson MarkLine


### Values

    addMarkLine :: forall e a. MarkLine -> EChart -> Eff (addMarkLineECharts :: AddMarkLine | e) EChart

    delMarkLine :: forall e. Number -> String -> EChart -> Eff (removeMarkLine :: RemoveMarkLine | e) EChart


## Module ECharts.Mark.Point

### Types

    newtype MarkPoint where
      MarkPoint :: { geoCoord :: Maybe (StrMap (Tuple Number Number)), "data" :: Maybe [MarkPointData], effect :: Maybe MarkPointEffect, large :: Maybe Boolean, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol } -> MarkPoint


### Type Class Instances

    instance markPointEncodeJson :: EncodeJson MarkPoint


### Values

    addMarkPoint :: forall e. MarkPoint -> EChart -> Eff (addMarkPointECharts :: AddMarkPoint | e) EChart

    delMarkPoint :: forall e. Number -> String -> EChart -> Eff (removeMarkPointECharts :: RemoveMarkPoint | e) EChart


## Module ECharts.Series.EventRiver

### Types

    newtype Evolution where
      Evolution :: { detail :: Maybe EvolutionDetail, value :: Number, time :: Date } -> Evolution

    newtype EvolutionDetail where
      EvolutionDetail :: { img :: Maybe String, text :: Maybe String, link :: Maybe String } -> EvolutionDetail

    newtype OneEvent where
      OneEvent :: { evolution :: Maybe [Evolution], weight :: Maybe Number, name :: Maybe String } -> OneEvent


### Type Class Instances

    instance evoDetailEncodeJson :: EncodeJson EvolutionDetail

    instance evoEncodeJson :: EncodeJson Evolution

    instance oneEventEncodeJson :: EncodeJson OneEvent


## Module ECharts.Series.Force

### Types

    newtype ForceCategory where
      ForceCategory :: { itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, name :: Maybe String } -> ForceCategory

    newtype Link where
      Link :: { itemStyle :: Maybe ItemStyle, weight :: Number, target :: LinkEnd, source :: LinkEnd } -> Link

    data LinkEnd where
      Name :: String -> LinkEnd
      Index :: Number -> LinkEnd

    type Matrix = [[Number]]

    newtype Node where
      Node :: { category :: Maybe Number, draggable :: Maybe Boolean, fixY :: Maybe Boolean, fixX :: Maybe Boolean, initial :: Maybe (Tuple Number Number), itemStyle :: Maybe ItemStyle, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol, ignore :: Maybe Boolean, value :: Number, label :: Maybe String, name :: Maybe String } -> Node


### Type Class Instances

    instance forceCategoryEncodeJson :: EncodeJson ForceCategory

    instance linkEncodeJson :: EncodeJson Link

    instance linkEndEncodeJson :: EncodeJson LinkEnd

    instance nodeEncodeJson :: EncodeJson Node


## Module ECharts.Series.Gauge

### Types

    newtype GaugeDetail where
      GaugeDetail :: { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, offsetCenter :: Maybe (Tuple PercentOrPixel PercentOrPixel), height :: Maybe Number, width :: Maybe Number, borderColor :: Maybe Color, borderWidth :: Maybe Number, backgroundColor :: Maybe Color, show :: Maybe Boolean } -> GaugeDetail

    newtype Pointer where
      Pointer :: { color :: Maybe Color, width :: Maybe Number, length :: Maybe Number } -> Pointer

    newtype SplitLine where
      SplitLine :: { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean } -> SplitLine


### Type Class Instances

    instance gaugeDetailEncodeJson :: EncodeJson GaugeDetail

    instance pointerEncodeJson :: EncodeJson Pointer

    instance splitLineEncodeJson :: EncodeJson SplitLine


## Module ECharts.Style.Area

### Types

    newtype AreaStyle where
      AreaStyle :: Color -> AreaStyle


### Type Class Instances

    instance areaStyleEncodeJson :: EncodeJson AreaStyle


## Module ECharts.Style.Checkpoint

### Types

    newtype CheckpointStyle where
      CheckpointStyle :: { label :: Maybe AxisLabel, borderColor :: Maybe Color, color :: Maybe Color, symbolSize :: Maybe SymbolSize, symbol :: Maybe Symbol } -> CheckpointStyle


### Type Class Instances

    instance checkpointStyleEncodeJson :: EncodeJson CheckpointStyle


## Module ECharts.Style.Chord

### Types

    newtype ChordStyle where
      ChordStyle :: { borderColor :: Maybe Color, borderWidth :: Maybe Number, color :: Maybe Color, width :: Maybe Number } -> ChordStyle


### Type Class Instances

    instance chordStyleJson :: EncodeJson ChordStyle


## Module ECharts.Style.Item

### Types

    newtype IStyle where
      IStyle :: { linkStyle :: Maybe LinkStyle, nodeStyle :: Maybe NodeStyle, chordStyle :: Maybe ChordStyle, areaStyle :: Maybe AreaStyle, lineStyle :: Maybe LineStyle, labelLine :: Maybe ItemLabelLine, label :: Maybe ItemLabel, barBorderWidth :: Maybe Number, barBorderRadius :: Maybe (Corner Number), barBorderColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe CalculableColor } -> IStyle

    newtype ItemLabel where
      ItemLabel :: { textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, distance :: Maybe Boolean, rotate :: Maybe Boolean, position :: Maybe LabelPosition, show :: Maybe Boolean } -> ItemLabel

    newtype ItemLabelLine where
      ItemLabelLine :: { lineStyle :: Maybe LineStyle, length :: Maybe Number, show :: Maybe Boolean } -> ItemLabelLine

    newtype ItemStyle where
      ItemStyle :: { emphasis :: Maybe IStyle, normal :: Maybe IStyle } -> ItemStyle


### Type Class Instances

    instance istyleEncodeJson :: EncodeJson IStyle

    instance itemLabelEncodeJson :: EncodeJson ItemLabel

    instance itemLabelLineEncodeJson :: EncodeJson ItemLabelLine

    instance itemStyleEncodeJson :: EncodeJson ItemStyle


## Module ECharts.Style.Line

### Types

    newtype LineStyle where
      LineStyle :: { shadowOffsetY :: Maybe Number, shadowOffsetX :: Maybe Number, shadowColor :: Maybe Color, width :: Maybe Number, "type" :: Maybe LineType, color :: Maybe Color } -> LineStyle

    data LineType where
      Solid :: LineType
      Dotted :: LineType
      Dashed :: LineType


### Type Class Instances

    instance lineStyleEncodeJson :: EncodeJson LineStyle

    instance linetypeEncodeJson :: EncodeJson LineType


## Module ECharts.Style.Link

### Types

    newtype LinkStyle where
      LinkStyle :: { width :: Maybe Number, color :: Maybe Color, "type" :: Maybe LinkType } -> LinkStyle

    data LinkType where
      LTCurve :: LinkType
      LTLine :: LinkType


### Type Class Instances

    instance linkStyleEncodeJson :: EncodeJson LinkStyle

    instance linkTypeEncodeJson :: EncodeJson LinkType


## Module ECharts.Style.Node

### Types

    newtype NodeStyle where
      NodeStyle :: { borderWidth :: Maybe Number, borderColor :: Maybe Color, color :: Maybe Color } -> NodeStyle


### Type Class Instances

    instance nodeStyleEncodeJson :: EncodeJson NodeStyle


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
      TextStyle :: { fontWeight :: Maybe FontWeight, fontStyle :: Maybe FontStyle, fontSize :: Maybe Number, fontFamily :: Maybe FontFamily, baseline :: Maybe TextBaseline, align :: Maybe HorizontalAlign, decoration :: Maybe Decoration, color :: Maybe Color } -> TextStyle


### Type Class Instances

    instance fontStyleEncodeJson :: EncodeJson FontStyle

    instance fontWeightEncodeJson :: EncodeJson FontWeight

    instance textBaselineEncodeJson :: EncodeJson TextBaseline

    instance textStyleEncodeJson :: EncodeJson TextStyle



