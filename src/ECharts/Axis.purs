module ECharts.Axis where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap
import Data.Maybe

import ECharts.Common
import ECharts.Coords
import ECharts.Color
import ECharts.Style.Line
import ECharts.Style.Text
import ECharts.Style.Area
import ECharts.Symbol
import ECharts.Formatter



newtype AxisLineStyle =
  AxisLineStyle {
    "color" :: Maybe [Tuple Number Color],
    "width" :: Maybe Number
    }
instance axisLineStyleEncodeJson :: EncodeJson AxisLineStyle where
  encodeJson (AxisLineStyle a) = fromObject $ fromList $ [
    "color" := a.color,
    "width" := a.width
    ]
axisLineStyleDefault = {
  color: Nothing,
  width: Nothing
  }


newtype AxisLine =
  AxisLine {
    "show" :: Maybe Boolean,
    "onZero" :: Maybe Boolean,
    "lineStyle" :: Maybe AxisLineStyle
    }
instance axisLineEncodeJson :: EncodeJson AxisLine where
  encodeJson (AxisLine a) = fromObject $ fromList $ [
    "show" := a.show,
    "lineStyle" := a.lineStyle,
    "onZero" := a.onZero
    ]
axisLineDefault = {
  show: Nothing,
  onZero: Nothing,
  lineStyle: Nothing
  }


newtype AxisTick =
  AxisTick {
    "show" :: Maybe Boolean,
    "splitNumber" :: Maybe Number,
    "length" :: Maybe Number,
    "lineStyle" :: Maybe LineStyle,
    "interval" :: Maybe Interval,
    "onGap" :: Maybe Boolean,
    "inside" :: Maybe Boolean
    }
instance axisTickEncodeJson :: EncodeJson AxisTick where
  encodeJson (AxisTick a) = fromObject $ fromList $ [
    "show" := a.show,
    "splitNumber" := a.splitNumber,
    "length" := a.length,
    "lineStyle" := a.lineStyle,
    "interval" := a.interval,
    "onGap" := a.onGap,
    "inside" := a.inside
    ]
axisTickDefault = {
  show: Nothing,
  splitNumber: Nothing,
  length: Nothing,
  lineStyle: Nothing,
  interval: Nothing,
  onGap: Nothing,
  inside: Nothing
  }


newtype AxisLabel =
  AxisLabel {
    "show" :: Maybe Boolean,
    "interval" :: Maybe Interval,
    "formatter" :: Maybe Formatter,
    "textStyle" :: Maybe TextStyle,
    "rotate" :: Maybe Number,
    "margin" :: Maybe Number,
    "clickable" :: Maybe Boolean
    }
instance axisLabelEncodeJson :: EncodeJson AxisLabel  where
  encodeJson (AxisLabel a) = fromObject $ fromList $ [
    "show" := a.show,
    "formatter" := a.formatter,
    "textStyle" := a.textStyle,
    "interval" := a.interval,
    "rotate" := a.rotate,
    "margin" := a.margin,
    "clickable" := a.clickable
    ]
axisLabelDefault = {
  show: Nothing,
  formatter: Nothing,
  textStyle: Nothing,
  interval: Nothing,
  rotate: Nothing,
  margin: Nothing,
  clickable: Nothing
  }


data Axises = OneAxis Axis | TwoAxises Axis Axis
instance axisesEncodeJson :: EncodeJson Axises where
  encodeJson (OneAxis axis) = encodeJson axis
  encodeJson (TwoAxises axis axis2) = encodeJson [axis, axis2]


newtype AxisSplitLine =
  AxisSplitLine {
    "show" :: Maybe Boolean,
    "onGap" :: Maybe Boolean,
    "lineStyle" :: Maybe LineStyle
    }
instance axisSplitLineEncodeJson :: EncodeJson AxisSplitLine where
  encodeJson (AxisSplitLine obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "onGap" := obj.onGap,
      "lineStyle" := obj.lineStyle
    ]
axisSplitLineDefault = {
  show: Nothing,
  onGap: Nothing,
  lineStyle: Nothing
  }


newtype AxisSplitArea =
  AxisSplitArea {
    "show" :: Maybe Boolean,
    "onGap" :: Maybe Boolean,
    "areaStyle" :: Maybe AreaStyle
    }
instance axisSplitAreaEncodeJson :: EncodeJson AxisSplitArea where
  encodeJson (AxisSplitArea obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "onGap" := obj.onGap,
      "areaStyle" := obj.areaStyle
    ]
axisSplitAreaDefault = {
  show: Nothing,
  onGap: Nothing,
  areaStyle: Nothing
  }

  
data AxisType = CategoryAxis | ValueAxis | TimeAxis
instance axisTypeEncodeJson :: EncodeJson AxisType where
  encodeJson a = encodeJson $ case a of
    CategoryAxis -> "category"
    ValueAxis -> "value"
    TimeAxis -> "time"


data AxisPosition = LeftAxis | RightAxis | TopAxis | BottomAxis
instance axisPositionEncodeJson :: EncodeJson AxisPosition where
  encodeJson a = encodeJson $ case a of
    LeftAxis -> "left"
    RightAxis -> "right"
    TopAxis -> "top"
    BottomAxis -> "bottom"


data AxisNameLocation = Start | End
instance axisNameLocationEncodeJson :: EncodeJson AxisNameLocation where
  encodeJson a = encodeJson $ case a of
    Start -> "start"
    End -> "end"



data AxisData = CommonAxisData String
              | CustomAxisData {"value" :: String, "textStyle" :: TextStyle}
instance axisDataEncodeJson :: EncodeJson AxisData where
  encodeJson (CommonAxisData name) = fromString name
  encodeJson (CustomAxisData obj) =
    fromObject $ fromList $
    [
      "value" := obj.value,
      "textStyle" := obj.textStyle
    ]


newtype Axis =
  Axis {
    "type" :: Maybe AxisType,
    "show" :: Maybe Boolean,
    "position" :: Maybe AxisPosition,
    "name" :: Maybe String,
    "nameLocation" :: Maybe AxisNameLocation,
    "nameTextStyle" :: Maybe TextStyle,
    "boundaryGap" :: Maybe (Tuple Number Number),
    "min" :: Maybe Number,
    "max" :: Maybe Number,
    "scale" :: Maybe Boolean,
    "splitNumber" :: Maybe Number,
    "axisLine" :: Maybe AxisLine,
    "axisTick" :: Maybe AxisTick,
    "axisLabel" :: Maybe AxisLabel,
    "splitLine" :: Maybe AxisSplitLine,
    "splitArea" :: Maybe AxisSplitArea,
    "data" :: Maybe [AxisData]
    }
axisDefault = {
  "type": Nothing,
  show: Nothing,
  position: Nothing,
  name: Nothing,
  nameLocation: Nothing,
  nameTextStyle: Nothing,
  boundaryGap: Nothing,
  min: Nothing,
  max: Nothing,
  scale: Nothing,
  splitNumber: Nothing,
  axisLine: Nothing,
  axisTick: Nothing,
  axisLabel: Nothing,
  splitLine: Nothing,
  splitArea: Nothing,
  data: Nothing
  }
instance axisEncJson :: EncodeJson Axis where
  encodeJson (Axis obj) =
    fromObject $ fromList $
    [
      "type" := obj.type,
      "show" := obj.show,
      "position" := obj.position,
      "name" := obj.name,
      "nameLocation" := obj.nameLocation,
      "nameTextStyle" := obj.nameTextStyle,
      "boundaryGap" := obj.boundaryGap,
      "min" := obj.min,
      "max" := obj.max,
      "scale" := obj.scale,
      "splitNumber" := obj.splitNumber,
      "axisLine" := obj.axisLine,
      "axisTick" := obj.axisTick,
      "axisLabel" := obj.axisLabel,
      "splitLine" := obj.splitLine,
      "splitArea" := obj.splitArea,
      "data" := obj.data
    ]


newtype PolarName =
  PolarName {
    "show" :: Maybe Boolean,
    "formatter" :: Maybe Formatter,
    "textStyle" :: Maybe TextStyle
    }
instance polarNameEncode :: EncodeJson PolarName where
  encodeJson (PolarName obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "formatter" := obj.formatter,
      "textStyle" := obj.textStyle
    ]
polarNameDefault = {
  show: Nothing,
  formatter: Nothing,
  textStyle: Nothing
  }


data PolarType = PolarPolygon | PolarCircle
instance polarTypeEncode :: EncodeJson PolarType where
  encodeJson a = encodeJson $ case a of
    PolarPolygon -> "polygon"
    PolarCircle -> "circle"



newtype Indicator =
  Indicator {
    "text" :: Maybe String,
    "min" :: Maybe Number,
    "max" :: Maybe Number,
    "axisLabel" :: Maybe AxisLabel
    }
instance indicatorEncodeJson :: EncodeJson Indicator where
  encodeJson (Indicator obj) =
    fromObject $ fromList $
    [
      "text" := obj.text,
      "min" := obj.min,
      "max" := obj.max,
      "axisLabel" := obj.axisLabel
    ]
indicatorDefault = {
  text: Nothing,
  min: Nothing,
  max: Nothing,
  axisLabel: Nothing
  }

newtype Polar =
  Polar {
    "center" :: Maybe (Tuple PercentOrPixel PercentOrPixel),
    "radius" :: Maybe PercentOrPixel,
    "startAngle" :: Maybe Number,
    "splitNumber" :: Maybe Number,
    "name" :: Maybe PolarName,
    "boundaryGap" :: Maybe (Tuple Number Number),
    "scale" :: Maybe Boolean,
    "axisLine" :: Maybe AxisLine,
    "axisLabel" :: Maybe AxisLabel,
    "splitLine" :: Maybe AxisSplitLine,
    "splitArea" :: Maybe AxisSplitArea,
    "type" :: Maybe PolarType,
    "indicator" :: Maybe [Indicator]
    }
instance polarEncodeJson :: EncodeJson Polar where
  encodeJson (Polar obj) =
    fromObject $ fromList $
    [
      "center" := obj.center,
      "radius" := obj.radius,
      "startAngle" := obj.startAngle,
      "splitNumber" := obj.splitNumber,
      "name" := obj.name,
      "boundaryGap" := obj.boundaryGap,
      "scale" := obj.scale,
      "axisLine" := obj.axisLine,
      "axisLabel" := obj.axisLabel,
      "splitLine" := obj.splitLine,
      "splitArea" := obj.splitArea,
      "type" := obj.type,
      "indicator" := obj.indicator
    ]
polarDefault = {
  center: Nothing,
  radius: Nothing,
  startAngle: Nothing,
  splitNumber: Nothing,
  name: Nothing,
  boundaryGap: Nothing,
  scale: Nothing,
  axisLine: Nothing,
  axisLabel: Nothing,
  splitLine: Nothing,
  splitArea: Nothing,
  type: Nothing,
  indicator: Nothing
  }
