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
  )

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
  , splitLine ∷ I
  , boundaryGap ∷ I
  | i)

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
  )

type GaugeI =
  ( name ∷ I
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
  )

type SplitLineI =
  ( show ∷ I )

type LabelI =
  ( normalLabel ∷ I
  , emphasisLabel ∷ I
  )

type LabelInnerI =
  ( show ∷ I
  , textStyle ∷ I
  )

type ItemsI =
  ( item ∷ I )
