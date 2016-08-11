module ECharts.Types.Phantom where

-- | Phantom effect for DSL
foreign import data I ∷ !
-- | Takes row of effects and returns an effect. Useful for emulating structural polymorphism.
foreign import data R ∷ (# ! → !)

-- | There is no some row constraints because they have no sense
-- | E.g. `coordinateSystem` field is fully determined by
-- | `xAxisIndex/yAxisIndex/polarIndex/radarIndex/etc`

-- | Open rows synonims are for mixins
-- | closed rows are for complete dsls

type PositionMixin i =
  ( left ∷ I
  , right ∷ I
  , bottom ∷ I
  , top ∷ I
  | i)

type ZMixin i =
  ( zlevel ∷ I
  , z ∷ I
  | i)

type SizeMixin i =
  ( width ∷ I
  , height ∷ I
  | i)

type BorderAndBackgroundMixin i =
  ( borderWidth ∷ I
  , borderColor ∷ I
  , backgroundColor ∷ I
  | i)

type ShadowMixin i =
  ( shadowBlur ∷ I
  , shadowOffsetX ∷ I
  , shadowOffsetY ∷ I
  , shadowColor ∷ I
  | i)

type BaseAnimationMixin i =
  ( animationDuration ∷ I
  , animationEasing ∷ I
  , animationDelay ∷ I
  | i)

type LargeMixin i =
  ( large ∷ I
  , largeThreshold ∷ I
  | i)

type AnimationMixin i =
  BaseAnimationMixin
  ( animation ∷ I
  , animationThreshold ∷ I
  , animationDurationUpdate ∷ I
  , animationEasingUpdate ∷ I
  , animationDelayUpdate ∷ I
  | i)

type MarkerMixin i =
  ( markPoint ∷ I
  , markLine ∷ I
  , markArea ∷ I
  | i)

type SymbolMixin i =
  ( symbol ∷ I
  , symbolSize ∷ I
  , symbolRotate ∷ I
  , symbolOffset ∷ I
  | i)

type MinMaxMixin i =
  ( min ∷ I
  , max ∷ I
  | i)

type NameStyleMixin i =
  ( nameLocation ∷ I
  , nameTextStyle ∷ I
  , nameGap ∷ I
  , nameRotate ∷ I
  | i)

type LegendHoverMixin i =
  ( legendHoverLink ∷ I
  , hoverAnimation ∷ I
  | i)


type NormalAndEmphasis i =
  ( normal ∷ R i
  , emphasis ∷ R i)

type LegendI =
  PositionMixin
  (ZMixin
   (SizeMixin
    (ShadowMixin
     (BorderAndBackgroundMixin
     ( show ∷ I
     , items ∷ I
     , orient ∷ I
     , align ∷ I
     , padding ∷ I
     , itemGap ∷ I
     , itemWidth ∷ I
     , itemHeight ∷ I
     , formatter ∷ I
     , inactiveColor ∷ I
     , selected ∷ I
     , textStyle ∷ I
     , selectedMode ∷ I
     , tooltip ∷ I)))))

type TooltipI =
  ZMixin
  (BorderAndBackgroundMixin
   ( show ∷ I
   , showContent ∷ I
   , trigger ∷ I
   , triggerOn ∷ I
   , alwaysShowContent ∷ I
   , showDelay ∷ I
   , hideDelay ∷ I
   , enterable ∷ I
   , tooltipPosition ∷ I
   , transitionDuration ∷ I
   , formatter ∷ I
   , animation ∷ I
   , textStyle ∷ I
   , padding ∷ I
   , extraCssText ∷ I --???
   , axisPointer ∷ I))

type CrossStyleI =
  ( color ∷ I
  , width ∷ I
  , lineType ∷ I
  , textStyle ∷ I)

type TitlesI =
  ( title ∷ I )

type TitleI =
  PositionMixin
  (ZMixin
   (ShadowMixin
    (BorderAndBackgroundMixin
     ( text ∷ I
     , textStyle ∷ I
     , offsetCenter ∷ I
     , show ∷ I
     , subtext ∷ I
     , link ∷ I
     , target ∷ I
     , textAlign ∷ I
     , textBaseline ∷ I
     , sublink ∷ I
     , subtarget ∷ I
     , subtextStyle ∷ I
     , padding ∷ I
     , itemGap ∷ I))))

type OptionI =
  AnimationMixin
  ( tooltip ∷ I
  , grid ∷ I
  , xAxis ∷ I
  , yAxis ∷ I
  , color ∷ I
  , series ∷ I
  , legend ∷ I
  , title ∷ I
  , backgroundColor ∷ I
  , brush ∷ I
  , toolbox ∷ I
  , visualMap ∷ I
  , radar ∷ I
  , polar ∷ I
  , radiusAxis ∷ I
  , angleAxis ∷ I
  , dataZoom ∷ I
  , geo ∷ I
  , parallel ∷ I
  , parallelAxis ∷ I
  , singleAxis ∷ I
  , timeline ∷ I
  , textStyle ∷ I
  , progressive ∷ I
  , progressiveThreshold ∷ I
  , blendMode ∷ I
  , hoverLayerThreshold ∷ I)

type TimelineI =
  PositionMixin
  (ZMixin
   (SymbolMixin
    ( show ∷ I
    , timelineType ∷ I
    , axisType ∷ I
    , currentIndex ∷ I
    , autoPlay ∷ I
    , rewind ∷ I
    , loop ∷ I
    , playInterval ∷ I
    , realtime ∷ I
    , controlPosition ∷ I
    , orient ∷ I
    , inverse ∷ I
    , lineStyle ∷ I
    , label ∷ I
    , itemStyle ∷ I
    , checkpointStyle ∷ I
    , controlStyle ∷ I
    , items ∷ I)))

type ParallelI =
  PositionMixin
  (ZMixin
   (SizeMixin
    ( layout ∷ I
    , axisExpandable ∷ I
    , axisExpandCenter ∷ I
    , axisExpandCount ∷ I
    , axisExpandWidth ∷ I
    , parallelAxisDefault ∷ I)))

type GeoI =
  PositionMixin
  (ZMixin
   ( show ∷ I
   , map ∷ I
   , roam ∷ I
   , center ∷ I
   , zoom ∷ I
   , scaleLimit ∷ I
   , nameMap ∷ I
   , selectedMode ∷ I
   , label ∷ I
   , itemStyle ∷ I
   , layoutCenter ∷ I
   , layoutSize ∷ I
   , regions ∷ I
   , silent ∷ I))

type PolarI =
  ZMixin
  ( center ∷ I
  , radius ∷ I)

type RadarsI =
  ( radar ∷ I )

type RadarI =
  ZMixin
  ( indicators ∷ I
  , shape ∷ I
  , splitNumber ∷ I
  , radarName ∷ I
  , splitLine ∷ I
  , splitArea ∷ I
  , axisLine ∷ I
  , center ∷ I
  , radius ∷ I
  , startAngle ∷ I
  , nameGap ∷ I
  , scale ∷ I
  , silent ∷ I
  , triggerEvent ∷ I
  , axisTick ∷ I
  , axisLabel ∷ I)

type IndicatorsI =
  ( indicator ∷ I )

type IndicatorI =
  MinMaxMixin
  ( name ∷ I)

type RadarNameI =
  ( show ∷ I
  , formatter ∷ I
  , textStyle ∷ I)


type DataZoomI =
  ( insideDataZoom ∷ I
  , sliderDataZoom ∷ I
  )

type DataZoomMixinI i =
  ( xAxisIndex ∷ I
  , yAxisIndex ∷ I
  , filterMode ∷ I
  , start ∷ I
  , end ∷ I
  , startValue ∷ I
  , endValue ∷ I
  , orient ∷ I
  , zoomLock ∷ I
  , throttle ∷ I
  | i)

type InsideDataZoomI = DataZoomMixinI ()

type SliderDataZoom =
  PositionMixin
  (ZMixin
   ( show ∷ I
   , backgroundColor ∷ I
   , dataBackground ∷ I
   , fillerColor ∷ I
   , borderColor ∷ I
   , handleIcon ∷ I
   , handleSize ∷ I
   , handleStyle ∷ I
   , labelPrecision ∷ I
   , labelFormatter ∷ I
   , showDetail ∷ I
   , showDataShadow ∷ I
   , realtime ∷ I
   , textStyle ∷ I))

type DataBackgroundI =
  ( lineStyle ∷ I
  , areaStyle ∷ I)

type HandleStyleI =
  ShadowMixin
  ( color ∷ I
  , borderColor ∷ I
  , borderWidth ∷ I
  , borderType ∷ I
  , opacity ∷ I)

type VisualMapI =
  ( continuousVisualMap ∷ I
  , piecewiseVisualMap ∷ I)

type ContinuousVisualMapI =
  PositionMixin
  (ZMixin
   (BorderAndBackgroundMixin
    (MinMaxMixin
     ( dimension ∷ I
     , textPair ∷ I
     , inverse ∷ I
     , itemHeight ∷ I
     , calculable ∷ I
     , inRange ∷ I
     , outOfRange ∷ I
     , controller ∷ I
     , orient ∷ I
     , range ∷ I
     , realtime ∷ I
     , precision ∷ I
     , itemWidth ∷ I
     , align ∷ I
     , textGap ∷ I
     , show ∷ I
     , seriesIndex ∷ I
     , hoverLink ∷ I
     , padding ∷ I
     , color ∷ I
     , textStyle ∷ I
     , formatter ∷ I))))

type PiecewiseVisualMapI =
  PositionMixin
  (ZMixin
   (BorderAndBackgroundMixin
    (MinMaxMixin
     ( splitNumber ∷ I
     , pieces ∷ I
     , categories ∷ I
     , selectedMode ∷ I
     , inverse ∷ I
     , precision ∷ I
     , itemWidth ∷ I
     , itemHeight ∷ I
     , align ∷ I
     , text ∷ I
     , textGap ∷ I
     , itemGap ∷ I
     , itemSymbol ∷ I
     , show ∷ I
     , dimension ∷ I
     , seriesIndex ∷ I
     , hoverLink ∷ I
     , outOfRange ∷ I
     , inRange ∷ I
     , controller ∷ I
     , orient ∷ I
     , padding ∷ I
     , color ∷ I
     , textStyle ∷ I
     , formatter ∷ I))))

type InOutRangeI =
  ( color ∷ I
  , symbol ∷ I
  , symbolSize ∷ I
  , colorAlpha ∷ I
  , opacity ∷ I
  , colorSaturation ∷ I
  , colorHue ∷ I
  , colorLightness ∷ I)

type ControllerI =
  ( inRange ∷ I
  , outOfRange ∷ I)

type ToolboxI =
  PositionMixin
  (ZMixin
   (SizeMixin
    ( feature ∷ I
    , show ∷ I
    , orient ∷ I
    , itemSize ∷ I
    , itemGap ∷ I
    , iconStyle ∷ I)))

type IconStyleI = NormalAndEmphasis IconStyleInnerI

type IconStyleInnerI =
  ShadowMixin
  ( color ∷ I
  , borderColor ∷ I
  , borderWidth ∷ I
  , borderType ∷ I
  , opacity ∷ I)

type FeatureI =
  ( brush ∷ I
  , saveAsImage ∷ I
  , restore ∷ I
  , dataView ∷ I
  , dataZoom ∷ I
  , magicType ∷ I)

type DataZoomFeatureI =
  ( show ∷ I
  , dzfTitle ∷ I
  , dzfIcon ∷ I
  , iconStyle ∷ I
  , xAxisIndex ∷ I
  , yAxisIndex ∷ I)

type DZFI =
  ( zoom ∷ I
  , back ∷ I)

type SaveAsImageI =
  ( imageType ∷ I
  , name ∷ I
  , backgroundColor ∷ I
  , excludeComponents ∷ I -- ???
  , show ∷ I
  , title ∷ I
  , icon ∷ I
  , iconStyle ∷ I
  , pixelRatio ∷ I)

type RestoreI =
  ( show ∷ I
  , title ∷ I
  , readOnly ∷ I
  , optionToContent ∷ I
  , contentToOption ∷ I
  , lang ∷ I
  , backgroundColor ∷ I
  , textareaColor ∷ I
  , textareaBorderColor ∷ I
  , textColor ∷ I
  , buttonColor ∷ I
  , buttonTextColor ∷ I
  , icon ∷ I
  , iconStyle ∷ I)

type DataViewI =
  ( show ∷ I
  , title ∷ I
  , icon ∷ I
  , iconStyle ∷ I
  , readOnly ∷ I)

type MagicTypeI =
  ( show ∷ I
  , mtTitle ∷ I
  , mtIcon ∷ I
  , mtOption ∷ I
  , mtSeriesIndex ∷ I
  , magics ∷ I )

type MTFieldI =
  ( line ∷ I
  , bar ∷ I
  , stack ∷ I
  , tiled ∷ I)

type BrushFeatureI =
  ( brushType ∷ I
  , bfIcon ∷ I
  , bfTitle ∷ I)

type BFFieldI =
  ( rect ∷ I
  , polygon ∷ I
  , lineX ∷ I
  , lineY ∷ I
  , keep ∷ I
  , clear ∷ I)


type MagicsI =
  ( magic ∷ I )

type BrushI =
  ( brushToolbox ∷ I
  , xAxisIndex ∷ I
  , brushLink ∷ I
  , seriesIndex ∷ I
  , geoIndex ∷ I
  , yAxisIndex ∷ I
  , brushType ∷ I
  , brushMode ∷ I
  , transformable ∷ I
  , brushStyle ∷ I
  , throttleType ∷ I
  , throttleDelay ∷ I
  , removeOnClick ∷ I
  , inBrush ∷ I
  , outOfBrush ∷ I)

type BrushToolboxI =
  ( tool ∷ I )

type GridI =
  PositionMixin
  (ZMixin
   (SizeMixin
    (ShadowMixin
     (BorderAndBackgroundMixin
      ( show ∷ I
      , textStyle ∷ I
      , containLabel ∷ I)))))

-- | There is no common serie thing, but special cases for
-- | every kind of series.
type SeriesI =
  ( pie ∷ I
  , line ∷ I
  , bar ∷ I
  , scatter ∷ I
  , effectScatter ∷ I
  , radarSeries ∷ I
  , treeMap ∷ I
  , boxPlot ∷ I
  , candlestick ∷ I
  , heatMap ∷ I
  , map ∷ I
  , parallelSeries ∷ I
  , lines ∷ I
  , graph ∷ I
  , sankey ∷ I
  , funnel ∷ I
  , gauge ∷ I
  , missing ∷ I)

-- | xAxis and yAxis has different position type
type AxisI i =
  ZMixin
  (MinMaxMixin
   ( axisType ∷ I
   , items ∷ I
   , axisTick ∷ I
   , axisLabel ∷ I
   , name ∷ I
   , scale ∷ I
   , boundaryGap ∷ I
   , silent ∷ I
   , splitLine ∷ I
   , splitArea ∷ I
   , axisLine ∷ I
   , interval ∷ I
   , inverse ∷ I
   , splitNumber ∷ I
   , minInterval ∷ I
   , triggerEvent ∷ I
   | i))

type SplitAreaI =
  ( show ∷ I
  , interval ∷ I
  , areaStyle ∷ I)

type AxisLineI =
  ( show ∷ I
  , onZero ∷ I
  , lineStyle ∷ I)

type YAxesI =
  ( addYAxis ∷ I )

type XAxesI =
  ( addXAxis ∷ I )

type ParallelAxisI =
  AxisI
  (NameStyleMixin
   ( dim ∷ I
   , parallelIndex ∷ I
   , realtime ∷ I))

type XAxisI =
  AxisI
  (NameStyleMixin
   ( horizontalPosition ∷ I
   , gridIndex ∷ I
   , offset ∷ I))

type YAxisI =
  AxisI
  (NameStyleMixin
   ( verticalPosition ∷ I
   , gridIndex ∷ I
   , offset ∷ I))

type RadiusAxisI =
  AxisI
  (NameStyleMixin
   ( polarIndex ∷ I))

type AngleAxisI =
  AxisI
  ( polarIndex ∷ I
  , startAngle ∷ I
  , clockwise ∷ I)

type SingleAxisI = AxisI (NameStyleMixin ())

type LineSeriesI =
  ZMixin
  (AnimationMixin
   (MarkerMixin
    (SymbolMixin
     (LegendHoverMixin
      ( name ∷ I
      , xAxisIndex ∷ I
      , yAxisIndex ∷ I
      , polarIndex ∷ I
      , showSymbol ∷ I
      , showAllSymbol ∷ I
      , connectNulls ∷ I
      , clipOverflow ∷ I
      , step ∷ I
      , lineStylePair ∷ I
      , itemStyle ∷ I
      , areaStylePair ∷ I
      , smooth ∷ I
      , smoothMonotone ∷ I
      , sampling ∷ I
      , items ∷ I
      , stack ∷ I
      , silent ∷ I
      , label ∷ I)))))

type BarSeriesI =
  ZMixin
  (AnimationMixin
   (MarkerMixin
    ( name ∷ I
    , items ∷ I
    , stack ∷ I
    , legendHoverLink ∷ I
    , xAxisIndex ∷ I
    , yAxisIndex ∷ I
    , itemStyle ∷ I
    , label ∷ I
    , barWidth ∷ I
    , barMaxWidth ∷ I
    , barMinHeight ∷ I
    , barGap ∷ I
    , barCategoryGap ∷ I)))

type PieSeriesI =
  ZMixin
  (AnimationMixin
   (LegendHoverMixin
    ( name ∷ I
    , center ∷ I
    , radius ∷ I
    , items ∷ I
    , startAngle ∷ I
    , selectedMode ∷ I
    , selectedOffset ∷ I
    , clockwise ∷ I
    , minAngle ∷ I
    , roseType ∷ I
    , avoidLabelOverlap ∷ I
    , label ∷ I
    , labelLine ∷ I
    , itemStyle ∷ I
    , silent ∷ I)))

type BasicScatterSeriesI i =
  MarkerMixin
  (ZMixin
   (AnimationMixin
    (SymbolMixin
     (LargeMixin
      (LegendHoverMixin
       ( name ∷ I
       , items ∷ I
       , itemStyle ∷ I
       , xAxisIndex ∷ I
       , yAxisIndex ∷ I
       , polarIndex ∷ I
       , geoIndex ∷ I
       , label ∷ I
       , silent ∷ I
       | i))))))

type ScatterI = BasicScatterSeriesI ()

type EffectScatterI =
  BasicScatterSeriesI
  ( effectType ∷ I
  , showEffectOn ∷ I
  , rippleEffect ∷ I)

type RadarSeriesI =
  ZMixin
  (AnimationMixin
   (SymbolMixin
    ( name ∷ I
    , items ∷ I
    , itemStyle ∷ I
    , lineStylePair ∷ I
    , areaStylePair ∷ I
    , axisLine ∷ I
    , radarIndex ∷ I
    , label ∷ I
    , silent ∷ I)))

type TreeMapI =
  PositionMixin
  (ZMixin
   (SizeMixin
    (BaseAnimationMixin
     ( squareRatio ∷ I
     , leafDepth ∷ I
     , roam ∷ I
     , nodeClick ∷ I
     , zoomNodeToRatio ∷ I
     , levels ∷ I
     , silentTreeMap ∷ I
     , visualDimension ∷ I
     , colorAlpha ∷ I
     , colorSaturation ∷ I
     , colorMappingBy ∷ I
     , visibleMin ∷ I
     , childrenVisibleMin ∷ I
     , label ∷ I
     , itemStyle ∷ I
     , breadcrumb ∷ I
     , items ∷ I))))

type BoxPlotI =
  ZMixin
  (BaseAnimationMixin
   (MarkerMixin
    (LegendHoverMixin
     ( xAxisIndex ∷ I
     , yAxisIndex ∷ I
     , name ∷ I
     , layout ∷ I
     , boxWidth ∷ I
     , itemStyle ∷ I
     , items ∷ I
     , silent ∷ I))))

type CandlestickI =
  ZMixin
  (BaseAnimationMixin
   (MarkerMixin
    (LegendHoverMixin
     ( name ∷ I
     , items ∷ I
     , xAxisIndex ∷ I
     , yAxisIndex ∷ I
     , layout ∷ I
     , itemStyle ∷ I))))

type HeatMapI =
  ZMixin
  (MarkerMixin
   ( name ∷ I
   , xAxisIndex ∷ I
   , yAxisIndex ∷ I
   , geoIndex ∷ I
   , blurSize ∷ I
   , minOpacity ∷ I
   , maxOpacity ∷ I
   , items ∷ I
   , label ∷ I
   , itemStyle ∷ I
   , silent ∷ I))

type MapI =
  PositionMixin
  (MarkerMixin
   (ZMixin
    ( name ∷ I
    , roam ∷ I
    , map ∷ I
    , center ∷ I
    , zoom ∷ I
    , scaleLimit ∷ I
    , nameMap ∷ I
    , selectedMode ∷ I
    , label ∷ I
    , itemStyle ∷ I
    , layoutCenter ∷ I
    , layoutSize ∷ I
    , mapValueCalculation ∷ I
    , showLegendSymbol ∷ I
    , silent ∷ I)))

type ScaleLimitI = MinMaxMixin ()

type RegionsI = (region ∷ I)

type RegionI =
  ( name ∷ I
  , selected ∷ I
  , itemStyle ∷ I
  , label ∷ I)

type ParallelSeriesI =
  AnimationMixin
  (ZMixin
   ( parallelIndex ∷ I
   , name ∷ I
   , lineStyle ∷ I
   , inactiveOpacity ∷ I
   , activeOpacity ∷ I
   , realtime ∷ I
   , items ∷ I
   , silent ∷ I))

type LinesI =
  ZMixin
  (AnimationMixin
   (MarkerMixin
    (LargeMixin
     ( name ∷ I
     , xAxisIndex ∷ I
     , yAxisIndex ∷ I
     , geoIndex ∷ I
     , polyline ∷ I
     , effect ∷ I
     , lineStyle ∷ I
     , label ∷ I
     , items ∷ I
     , silent ∷ I))))

type GraphI =
  AnimationMixin
  (ZMixin
   (PositionMixin
    (SizeMixin
     (SymbolMixin
      (MarkerMixin
       (LegendHoverMixin
        ( name ∷ I
        , xAxisIndex ∷ I
        , yAxisIndex ∷ I
        , polarIndex ∷ I
        , geoIndex ∷ I
        , layout ∷ I
        , force ∷ I
        , roam ∷ I
        , nodeScaleRatio ∷ I
        , draggable ∷ I
        , focusNodeAdjancency ∷ I
        , itemStyle ∷ I
        , lineStylePair ∷ I
        , label ∷ I
        , categories ∷ I
        , items ∷ I
        , nodes ∷ I
        , links ∷ I
        , edges ∷ I
        , edgeLabel ∷ I
        , edgeSymbols ∷ I
        , edgeSymbolSize ∷ I
        , silent ∷ I)))))))


type EdgeLabelI = NormalAndEmphasis EdgeLabelInnerI

type EdgeLabelInnerI =
  ( show ∷ I
  , edgeLabelPosition ∷ I
  , formatter ∷ I
  , textStyle ∷ I )

type LinksI =
  ( link ∷ I )

type LinkI =
  ( source ∷ I
  , target ∷ I
  , label ∷ I
  , symbol ∷ I
  , lineStylePair ∷ I
  , symbolSize ∷ I)

type EdgeSymbolsI =
  ( edgeSymbol ∷ I )

type SankeyI =
  AnimationMixin
  (ZMixin
   (SizeMixin
    (PositionMixin
     ( nodeWidth ∷ I
     , nodeGap ∷ I
     , layoutIterations ∷ I
     , label ∷ I
     , itemStyle ∷ I
     , lineStyle ∷ I
     , items ∷ I
     , nodes ∷ I
     , links ∷ I
     , edges ∷ I
     , silent ∷ I ))))

type FunnelI =
  AnimationMixin
  (MarkerMixin
   -- positions and sizes are used in examples but aren't documented
   (PositionMixin
    (SizeMixin
     (MinMaxMixin
     ( name ∷ I
     , label ∷ I
     , labelLine ∷ I
     , items ∷ I
     , itemStyle ∷ I
     , minSize ∷ I
     , maxSize ∷ I
     , sort ∷ I
     , gap ∷ I
     , legendHoverLink ∷ I
     , funnelAlign ∷ I
     , silent ∷ I)))))

type GaugeI =
  AnimationMixin
  (ZMixin
   (MarkerMixin
    (MinMaxMixin
    ( name ∷ I
    -- deprecated ??
    , gaugeRadius ∷ I
    , center ∷ I
    , radius ∷ I
    , startAngle ∷ I
    , endAngle ∷ I
    , clockwise ∷ I
    , splitNumber ∷ I
    , axisLine ∷ I
    , splitLine ∷ I
    , axisTick ∷ I
    , axisLabel ∷ I
    , gaugePointer ∷ I
    , itemStyle ∷ I
    , items ∷ I
    , title ∷ I
    , detail ∷ I
    , silent ∷ I ))))

type GaugePointerI =
  ( show ∷ I
  , length ∷ I
  , width ∷ I)

type ItemI =
  SymbolMixin
  ( name ∷ I
  , treeMapNodeId ∷ I
  , value ∷ I
  , label ∷ I
  , labelLine ∷ I
  , itemStyle ∷ I
  , lineStyle ∷ I
  , areaStyle ∷ I
  , selected ∷ I
  , icon ∷ I
  , color ∷ I
  , colorAlpha ∷ I
  , colorSaturation ∷ I
  , colorMappingBy ∷ I
  , x ∷ I
  , y ∷ I
  , visualDimension ∷ I
  , visibleMin ∷ I
  , childrenVisibleMin ∷ I
  , coord ∷ I
  , coords ∷ I
  , category ∷ I
  , valueIndex ∷ I
  , valueDim ∷ I
  , markType ∷ I)

type Coords =
  ( coord ∷ I )

type AxisPointerI =
  AnimationMixin
  ( show ∷ I
  , axis ∷ I
  , pointerType ∷ I
  , axisPointerType ∷ I
  , shadowStyle ∷ I
  , lineStyle ∷ I
  , crossStyle ∷ I)

type ShadowStyleI =
  ShadowMixin
  ( color ∷ I
  , opacity ∷ I)

type LineStylePairI = NormalAndEmphasis LineStyleI

type LineStyleI =
  ShadowMixin
  ( lineType ∷ I
  , width ∷ I
  , color ∷ I
  , opacity ∷ I
  , curveness ∷ I)

type SplitLineI =
  ( show ∷ I
  , lineStyle ∷ I
  , interval ∷ I
  , length ∷ I)

type LabelI = NormalAndEmphasis LabelInnerI

type LabelInnerI =
  ( show ∷ I
  , textStyle ∷ I
  , position ∷ I
  , formatter ∷ I
  , color ∷ I)

type ItemsI =
  ( item ∷ I )

type ItemStyleI = NormalAndEmphasis IStyleI

type IStyleI =
  ShadowMixin
  ( barBorderWidth ∷ I
  , opacity ∷ I
  , borderWidth ∷ I
  , borderColor ∷ I
  , color ∷ I)

type TextStyleI =
  ( color ∷ I
  , fontWeight ∷ I
  , fontSize ∷ I
  , fontStyle ∷ I
  , fontFamily ∷ I)

type AreaStylePairI = NormalAndEmphasis AreaStyleI

type AreaStyleI =
  ShadowMixin
  ( color ∷ I
  , opacity ∷ I)

type AxisTickI =
  ( show ∷ I
  , length ∷ I
  , inside ∷ I
  , interval ∷ I
  , alignWithLabel ∷ I
  , lineStyle ∷ I
  , splitNumber ∷ I)

type AxisLabelI =
  ( show ∷ I
  , formatter ∷ I
  , interval ∷ I
  , inside ∷ I
  , margin ∷ I
  , rotate ∷ I
  , textStyle ∷ I)

type DetailI =
  ( textStyle ∷ I
  , show ∷ I)

type LabelLineI = NormalAndEmphasis LabelLineInnerI

type LabelLineInnerI =
  ( show ∷ I )

type ValuesI =
  ( addValue ∷ I )

type MarkPointI =
  AnimationMixin
  (SymbolMixin
   ( items ∷ I
   , label ∷ I
   , itemStyle ∷ I
   , silent ∷ I))

type MarkLineI =
  AnimationMixin
  (SymbolMixin
   ( precision ∷ I
   , label ∷ I
   , silent ∷ I
   , lineStylePair ∷ I
   , items ∷ I))

type MarkAreaI =
  AnimationMixin
  ( label ∷ I
  , itemStyle ∷ I
  , items ∷ I)

type RippleEffectI =
  ( period ∷ I
  , scale ∷ I
  , brushType ∷ I)

type LevelsI = (level ∷ I)

type LevelI =
  ( visualDimension ∷ I
  , color ∷ I
  , colorAlpha ∷ I
  , colorSaturation ∷ I
  , colorMappingBy ∷ I
  , visibleMin ∷ I
  , childrenVisibleMin ∷ I
  , label ∷ I
  , itemStyle ∷ I)

type EffectI =
  ( show ∷ I
  , period ∷ I
  , constantSpeed ∷ I
  , symbol ∷ I
  , symbolSize ∷ I
  , color ∷ I
  , trailLength ∷ I
  , loop ∷ I)

type ForceI =
  ( initLayout ∷ I
  , repulsion ∷ I
  , gravity ∷ I
  , edgeLength ∷ I
  , layoutAnimation ∷ I)

type CategoriesI = (category ∷ I)

type CategoryI =
  SymbolMixin
  ( name ∷ I
  , itemStyle ∷ I
  , label ∷ I)
