module ECharts.Types.Phantom where

-- | Phantom effect for DSL
foreign import data I ∷ !

-- | Note that open rows type synonims is for mixins and
-- | closed rows are for complete dsls

type PositionedI i =
  ( left ∷ I
  , right ∷ I
  , bottom ∷ I
  , top ∷ I
  | i )

type LegendI =
  PositionedI
    ( show ∷ I
    , items ∷ I
    , orient ∷ I
    , align ∷ I
    )

type TooltipI =
  ( show ∷ I
  , showContent ∷ I
  , trigger ∷ I
  , showDelay ∷ I
  , axisPointer ∷ I
  , zlevel ∷ I
  , formatter ∷ I
  , animation ∷ I
  )

type TitleI =
  ( text ∷ I
  , left ∷ I
  , textStyle ∷ I
  , offsetCenter ∷ I
  , show ∷ I
  , subtext ∷ I
  )

type OptionI =
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
  )

type VisualMapI =
  ( continuousVisualMap ∷ I
  , piecewiseVisualMap ∷ I
  )

type ContinuousVisualMapI =
  PositionedI
  ( dimension ∷ I
  , textPair ∷ I
  , inverse ∷ I
  , itemHeight ∷ I
  , calculable ∷ I
  , min ∷ I
  , max ∷ I
  , inRange ∷ I
  , outOfRange ∷ I
  , controller ∷ I
  )

type InOutRangeI =
  ( color ∷ I
  , colorLightness ∷ I
  )

type ControllerI =
  ( inRange ∷ I
  , outOfRange ∷ I
  )

type ToolboxI =
  PositionedI
  ( feature ∷ I
  , show ∷ I
  , orient ∷ I
  )

type FeatureI =
  ( brush ∷ I
  , saveAsImage ∷ I
  , restore ∷ I
  , dataView ∷ I
  , dataZoom ∷ I
  , magicType ∷ I
  )

type SaveAsImageI =
  ( imageType ∷ I
  , name ∷ I
  , backgroundColor ∷ I
  , excludeComponents ∷ I -- ???
  , show ∷ I
  , title ∷ I
  , icon ∷ I
  , iconStyle ∷ I
  , pixelRatio ∷ I
  )

type RestoreI =
  ( show ∷ I
  , title ∷ I
  , icon ∷ I
  , iconStyle ∷ I
  )

type DataViewI =
  ( show ∷ I
  , title ∷ I
  , icon ∷ I
  , iconStyle ∷ I
  , readOnly ∷ I
  )

type MagicTypeI =
  ( show ∷ I
  , magics ∷ I
  )

-- LOL
type MagicsI =
  ( magic ∷ I )


type BrushI =
  ( brushToolbox ∷ I
  , xAxisIndex ∷ I
  )

type BrushToolboxI =
  ( tool ∷ I )


type GridI =
  PositionedI
    ( show ∷ I
    , textStyle ∷ I
    )

-- | There is no common serie thing, but special cases for
-- | every kind of series.
type SeriesI =
  ( pie ∷ I
  , line ∷ I
  , bar ∷ I
  , scatter ∷ I
  , effectScatter ∷ I
  , radar ∷ I
  , treeMap ∷ I
  , boxPlot ∷ I
  , candlestick ∷ I
  , heatMap ∷ I
  , map ∷ I
  , parallel ∷ I
  , lines ∷ I
  , graph ∷ I
  , sankey ∷ I
  , funnel ∷ I
  , gauge ∷ I
  )


-- | xAxis and yAxis has different position type
type AxisI i =
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
  , inverse ∷ I
  | i)

type SplitAreaI =
  ( show ∷ I )

type AxisLineI =
  ( onZero ∷ I
  , lineStyle ∷ I
  )


type XAxisI =
  AxisI (horizontalPosition ∷ I)

type YAxisI =
  AxisI (verticalPosition ∷ I)

type LineSeriesI =
  ( name ∷ I
  , xAxisIndex ∷ I
  , yAxisIndex ∷ I
  , polarIndex ∷ I
  , symbol ∷ I
  , symbolSize ∷ I
  , lineStyle ∷ I
  , itemStyle ∷ I
  , areaStyle ∷ I
  , smooth ∷ I
  , items ∷ I
  , hoverAnimation ∷ I
  , showSymbol ∷ I
  )

type BarSeriesI =
  ( name ∷ I
  , items ∷ I
  , stack ∷ I
  , itemStyle ∷ I
  )

type PieSeriesI =
  ( name ∷ I
  , center ∷ I
  , radius ∷ I
  , items ∷ I
  , startAngle ∷ I
  , selectedMode ∷ I
  , label ∷ I
  )

type ScatterI =
  ( name ∷ I
  , items ∷ I
  , large ∷ I
  , symbolSize ∷ I
  )

type EffectScatterI =
  ( name ∷ I
  )

type RadarI =
  ( name ∷ I
  )

type TreeMapI =
  ( name ∷ I
  )

type BoxPlotI =
  ( name ∷ I
  )

type CandlestickI =
  ( name ∷ I
  )

type HeatMapI =
  ( name ∷ I
  )

type MapI =
  ( name ∷ I
  )

type ParallelI =
  ( name ∷ I
  )

type LinesI =
  ( name ∷ I
  )

type GraphI =
  ( name ∷ I
  )

type SankeyI =
  ( name ∷ I
  )

type FunnelI =
  ( name ∷ I
  , left ∷ I
  , width ∷ I
  , label ∷ I
  , labelLine ∷ I
  , items ∷ I
  , itemStyle ∷ I
  , maxSize ∷ I
  )

type GaugeI =
  ( name ∷ I
  , z ∷ I
  , min ∷ I
  , max ∷ I
  , splitNumber ∷ I
  , gaugeRadius ∷ I
  , axisLine ∷ I
  , axisTick ∷ I
  , splitLine ∷ I
  , title ∷ I
  , detail ∷ I
  , items ∷ I
  , center ∷ I
  , endAngle ∷ I
  , gaugePointer ∷ I
  , startAngle ∷ I
  , axisLabel ∷ I
  )

type GaugePointerI =
  ( show ∷ I
  , length ∷ I
  , width ∷ I
  )

type ItemI =
  ( name ∷ I
  , value ∷ I
  , symbol ∷ I
  , symbolSize ∷ I
  , symbolRotate ∷ I
  , symbolOffset ∷ I
  , label ∷ I
  , itemStyle ∷ I
  , selected ∷ I
  )

type AxisPointerI =
  ( show ∷ I
  , pointerType ∷ I
  , lineStyle ∷ I
  , zlevel ∷ I
  )

type LineStyleI =
  ( lineType ∷ I
  , width ∷ I
  , color ∷ I
  )

type SplitLineI =
  ( show ∷ I
  , lineStyle ∷ I
  , length ∷ I
  )

type LabelI =
  ( normalLabel ∷ I
  , emphasisLabel ∷ I
  )

type LabelInnerI =
  ( show ∷ I
  , textStyle ∷ I
  , position ∷ I
  , formatter ∷ I
  , color ∷ I
  )

type ItemsI =
  ( item ∷ I )

type ItemStyleI =
  ( normalItemStyle ∷ I
  , emphasisItemStyle ∷ I
  )

type IStyleI =
  ( barBorderWidth ∷ I
  , shadowBlur ∷ I
  , shadowOffsetX ∷ I
  , shadowOffsetY ∷ I
  , shadowColor ∷ I
  , opacity ∷ I
  , borderWidth ∷ I
  , borderColor ∷ I
  )

type TextStyleI =
  ( color ∷ I
  , fontWeight ∷ I
  , fontSize ∷ I
  , fontStyle ∷ I
  )

type AreaStyleI =
  ( color ∷ I
  )

type AxisTickI =
  ( show ∷ I
  , length ∷ I
  , lineStyle ∷ I
  , splitNumber ∷ I
  )

type AxisLabelI =
  ( show ∷ I
  , formatter ∷ I
  )

type DetailI =
  ( textStyle ∷ I
  , show ∷ I
  )

type LabelLineI =
  ( normalLabelLine ∷ I
  , emphasisLabelLine ∷ I
  )

type LabelLineInnerI =
  ( show ∷ I )
