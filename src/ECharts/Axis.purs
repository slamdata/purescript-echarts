module ECharts.Axis where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap
import Data.Maybe
import Data.Either
import Control.Alt ((<|>))

import ECharts.Common
import ECharts.Coords
import ECharts.Color
import ECharts.Style.Line
import ECharts.Style.Text
import ECharts.Style.Area
import ECharts.Symbol
import ECharts.Formatter


type AxisLineStyleRec = {
    color :: Maybe [Tuple Number Color],
    width :: Maybe Number
    }

newtype AxisLineStyle = AxisLineStyle AxisLineStyleRec

instance axisLineStyleEncodeJson :: EncodeJson AxisLineStyle where
  encodeJson (AxisLineStyle a) = fromObject $ fromList $ [
    "color" := a.color,
    "width" := a.width
    ]

instance axisLineStyleDecodeJson :: DecodeJson AxisLineStyle where
  decodeJson j = do
    o <- decodeJson j
    r <- {color: _, width: _} <$> (o .? "color") <*> (o .? "width")
    pure $ AxisLineStyle r


axisLineStyleDefault :: AxisLineStyleRec
axisLineStyleDefault = {
  color: Nothing,
  width: Nothing
  }


type AxisLineRec = {
    show :: Maybe Boolean,
    onZero :: Maybe Boolean,
    lineStyle :: Maybe AxisLineStyle
    }

newtype AxisLine = AxisLine AxisLineRec
   
instance axisLineEncodeJson :: EncodeJson AxisLine where
  encodeJson (AxisLine a) = fromObject $ fromList $ [
    "show" := a.show,
    "lineStyle" := a.lineStyle,
    "onZero" := a.onZero
    ]

instance axisLineDecodeJson :: DecodeJson AxisLine where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , lineStyle: _
         , onZero: _} <$>
         (o .? "show") <*>
         (o .? "lineStyle") <*>
         (o .? "onZero")
    pure $ AxisLine r

axisLineDefault :: AxisLineRec                            
axisLineDefault = {
  show: Nothing,
  onZero: Nothing,
  lineStyle: Nothing
  }


type AxisTickRec = {
    show :: Maybe Boolean,
    splitNumber :: Maybe Number,
    length :: Maybe Number,
    lineStyle :: Maybe LineStyle,
    interval :: Maybe Interval,
    onGap :: Maybe Boolean,
    inside :: Maybe Boolean
    }

newtype AxisTick = AxisTick AxisTickRec
   
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

instance axisTickDecodeJson :: DecodeJson AxisTick where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , splitNumber: _
         , length: _
         , lineStyle: _
         , interval: _
         , onGap: _
         , inside: _ } <$>
         (o .? "show") <*>
         (o .? "splitNumber") <*>
         (o .? "length") <*>
         (o .? "lineStyle") <*>
         (o .? "interval") <*>
         (o .? "onGap") <*>
         (o .? "interval")
    pure $ AxisTick r
         
                            
axisTickDefault :: AxisTickRec
axisTickDefault = {
  show: Nothing,
  splitNumber: Nothing,
  length: Nothing,
  lineStyle: Nothing,
  interval: Nothing,
  onGap: Nothing,
  inside: Nothing
  }

type AxisLabelRec =  {
    show :: Maybe Boolean,
    interval :: Maybe Interval,
    formatter :: Maybe Formatter,
    textStyle :: Maybe TextStyle,
    rotate :: Maybe Number,
    margin :: Maybe Number,
    clickable :: Maybe Boolean
    }

newtype AxisLabel = AxisLabel AxisLabelRec
  
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

instance axisLabelDecodeJson :: DecodeJson AxisLabel where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , formatter: _
         , textStyle: _
         , interval: _
         , rotate: _
         , margin: _
         , clickable: _ } <$>
         (o .? "show") <*>
         (o .? "formatter") <*>
         (o .? "textStyle") <*>
         (o .? "interval") <*>
         (o .? "rotate") <*>
         (o .? "margin") <*>
         (o .? "clickable")
    pure $ AxisLabel r
                             
axisLabelDefault :: AxisLabelRec
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

instance axisesDecodeJson :: DecodeJson Axises where
  decodeJson j =
    (do arr <- decodeJson j
        case arr of
          a:b:[] -> pure $ TwoAxises a b
          _ -> Left "incorrect axises") <|>
    (OneAxis <$> decodeJson j)

type AxisSplitLineRec = {
    show :: Maybe Boolean,
    onGap :: Maybe Boolean,
    lineStyle :: Maybe LineStyle
    }

newtype AxisSplitLine = AxisSplitLine AxisSplitLineRec
   
instance axisSplitLineEncodeJson :: EncodeJson AxisSplitLine where
  encodeJson (AxisSplitLine obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "onGap" := obj.onGap,
      "lineStyle" := obj.lineStyle
    ]

instance axisSplitLineDecodeJson :: DecodeJson AxisSplitLine where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , onGap: _
         , lineStyle: _ } <$>
         (o .? "show") <*>
         (o .? "onGap") <*>
         (o .? "lineStyle")
    pure $ AxisSplitLine r

axisSplitLineDefault :: AxisSplitLineRec
axisSplitLineDefault = {
  show: Nothing,
  onGap: Nothing,
  lineStyle: Nothing
  }


type AxisSplitAreaRec =  {
    show :: Maybe Boolean,
    onGap :: Maybe Boolean,
    areaStyle :: Maybe AreaStyle
    }


newtype AxisSplitArea = AxisSplitArea AxisSplitAreaRec
  
instance axisSplitAreaEncodeJson :: EncodeJson AxisSplitArea where
  encodeJson (AxisSplitArea obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "onGap" := obj.onGap,
      "areaStyle" := obj.areaStyle
    ]

instance axisSplitAreaDecodeJson :: DecodeJson AxisSplitArea where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , onGap: _
         , areaStyle: _ } <$>
         (o .? "show") <*>
         (o .? "onGap") <*>
         (o .? "areaStyle")
    pure $ AxisSplitArea r
          

axisSplitAreaDefault :: AxisSplitAreaRec
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

instance axisTypeDecodeJson :: DecodeJson AxisType where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "category" -> pure CategoryAxis
      "value" -> pure ValueAxis
      "time" -> pure TimeAxis
      _ -> Left "incorrect axis type"


data AxisPosition = LeftAxis | RightAxis | TopAxis | BottomAxis
instance axisPositionEncodeJson :: EncodeJson AxisPosition where
  encodeJson a = encodeJson $ case a of
    LeftAxis -> "left"
    RightAxis -> "right"
    TopAxis -> "top"
    BottomAxis -> "bottom"

instance axisPositionDecodeJson :: DecodeJson AxisPosition where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "left" -> pure LeftAxis
      "right" -> pure RightAxis
      "top" -> pure TopAxis
      "bottom" -> pure BottomAxis
      _ -> Left "incorrect axis position"


data AxisNameLocation = Start | End
instance axisNameLocationEncodeJson :: EncodeJson AxisNameLocation where
  encodeJson a = encodeJson $ case a of
    Start -> "start"
    End -> "end"

instance axisNameLocationDecodeJson :: DecodeJson AxisNameLocation where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "start" -> pure Start
      "end" -> pure End
      _ -> Left "incorrect axis name location"
      


type CustomAxisDataRec = {
  value :: String,
  textStyle :: TextStyle
  }

data AxisData = CommonAxisData String
              | CustomAxisData CustomAxisDataRec
instance axisDataEncodeJson :: EncodeJson AxisData where
  encodeJson (CommonAxisData name) = fromString name
  encodeJson (CustomAxisData obj) =
    fromObject $ fromList $
    [
      "value" := obj.value,
      "textStyle" := obj.textStyle
    ]

instance axisDataDecodeJson :: DecodeJson AxisData where
  decodeJson j =
    (CommonAxisData <$> decodeJson j) <|>
    (do o <- decodeJson j
        r <- {value: _, textStyle: _} <$> (o .? "value") <*> (o .? "textStyle")
        pure $ CustomAxisData r)



data AxisBoundaryGap = CatBoundaryGap Boolean
                   | ValueBoundaryGap Number Number

instance axisBoundaryGapEncodeJson :: EncodeJson AxisBoundaryGap where
  encodeJson (CatBoundaryGap bool) = encodeJson bool
  encodeJson (ValueBoundaryGap num1 num2) = encodeJson [num1, num2]

instance axisBoundaryGapDecodeJson :: DecodeJson AxisBoundaryGap where
  decodeJson j =
    (CatBoundaryGap <$> decodeJson j) <|>
    (do arr <- decodeJson j
        case arr of
          a:b:[] -> pure $ ValueBoundaryGap a b
          _ -> Left "incorrect axis boundary gap")


type AxisRec = {
    "type" :: Maybe AxisType,
    show :: Maybe Boolean,
    position :: Maybe AxisPosition,
    name :: Maybe String,
    nameLocation :: Maybe AxisNameLocation,
    nameTextStyle :: Maybe TextStyle,
    boundaryGap :: Maybe AxisBoundaryGap,
    min :: Maybe Number,
    max :: Maybe Number,
    scale :: Maybe Boolean,
    splitNumber :: Maybe Number,
    axisLine :: Maybe AxisLine,
    axisTick :: Maybe AxisTick,
    axisLabel :: Maybe AxisLabel,
    splitLine :: Maybe AxisSplitLine,
    splitArea :: Maybe AxisSplitArea,
    "data" :: Maybe [AxisData]
    }

newtype Axis = Axis AxisRec

axisDefault :: AxisRec
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
  "data": Nothing
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

instance axisDecJson :: DecodeJson Axis where
  decodeJson j = do
    o <- decodeJson j
    r <- { "type": _
         , show: _
         , position: _
         , name: _
         , nameLocation: _
         , nameTextStyle: _
         , boundaryGap: _
         , min: _
         , max: _
         , scale: _
         , splitNumber: _
         , axisLine: _
         , axisTick: _
         , axisLabel: _
         , splitLine: _
         , splitArea: _
         , "data": _ } <$>
         (o .? "type") <*>
         (o .? "show") <*>
         (o .? "position") <*>
         (o .? "name") <*>
         (o .? "nameLocation") <*>
         (o .? "nameTextStyle") <*>
         (o .? "boundaryGap") <*>
         (o .? "min") <*>
         (o .? "max") <*>
         (o .? "scale") <*>
         (o .? "splitNumber") <*>
         (o .? "axisLine") <*>
         (o .? "axisTick") <*>
         (o .? "axisLabel") <*>
         (o .? "splitLine") <*>
         (o .? "splitArea") <*>
         (o .? "data")
    pure $ Axis r


type PolarNameRec = {
    show :: Maybe Boolean,
    formatter :: Maybe Formatter,
    textStyle :: Maybe TextStyle
    }

newtype PolarName = PolarName PolarNameRec
   
instance polarNameEncode :: EncodeJson PolarName where
  encodeJson (PolarName obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "formatter" := obj.formatter,
      "textStyle" := obj.textStyle
    ]

instance polarNameDecodeJson :: DecodeJson PolarName where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , formatter: _
         , textStyle: _ } <$>
         (o .? "show") <*>
         (o .? "formatter") <*>
         (o .? "textStyle")
    pure $ PolarName r

polarNameDefault :: PolarNameRec
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

instance polarTypeDecodeJson :: DecodeJson PolarType where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "polygon" -> pure PolarPolygon
      "circle" -> pure PolarCircle
      _ -> Left "incorrect polar type"


type IndicatorRec = {
    text :: Maybe String,
    min :: Maybe Number,
    max :: Maybe Number,
    axisLabel :: Maybe AxisLabel
    }

newtype Indicator = Indicator IndicatorRec

instance indicatorEncodeJson :: EncodeJson Indicator where
  encodeJson (Indicator obj) =
    fromObject $ fromList $
    [
      "text" := obj.text,
      "min" := obj.min,
      "max" := obj.max,
      "axisLabel" := obj.axisLabel
    ]

instance indicatorDecodeJson :: DecodeJson Indicator where
  decodeJson j = do
    o <- decodeJson j
    r <- { text: _
         , min: _
         , max: _
         , axisLabel: _ } <$>
         (o .? "text") <*>
         (o .? "min") <*>
         (o .? "max") <*>
         (o .? "axisLabel")
    pure $ Indicator r
         

    
indicatorDefault :: IndicatorRec
indicatorDefault = {
  text: Nothing,
  min: Nothing,
  max: Nothing,
  axisLabel: Nothing
  }

type PolarRec =  {
    center :: Maybe (Tuple PercentOrPixel PercentOrPixel),
    radius :: Maybe PercentOrPixel,
    startAngle :: Maybe Number,
    splitNumber :: Maybe Number,
    name :: Maybe PolarName,
    boundaryGap :: Maybe (Tuple Number Number),
    scale :: Maybe Boolean,
    axisLine :: Maybe AxisLine,
    axisLabel :: Maybe AxisLabel,
    splitLine :: Maybe AxisSplitLine,
    splitArea :: Maybe AxisSplitArea,
    "type" :: Maybe PolarType,
    indicator :: Maybe [Indicator]
    }

newtype Polar = Polar PolarRec

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

instance polarDecodeJson :: DecodeJson Polar where
  decodeJson j = do
    o <- decodeJson j
    r <- { center: _
         , radius: _
         , startAngle: _
         , splitNumber: _
         , name: _
         , boundaryGap: _
         , scale: _
         , axisLine: _
         , axisLabel: _
         , splitLine: _
         , splitArea: _
         , "type": _
         , indicator: _ } <$>
         (o .? "center") <*>
         (o .? "radius") <*>
         (o .? "startAngle") <*>
         (o .? "splitNumber") <*>
         (o .? "name") <*>
         (o .? "boundaryGap") <*>
         (o .? "scale") <*>
         (o .? "axisLine") <*>
         (o .? "axisLabel") <*>
         (o .? "splitLine") <*>
         (o .? "splitArea") <*>
         (o .? "type") <*>
         (o .? "indicator")
    pure $ Polar r



polarDefault :: PolarRec
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
  "type": Nothing,
  indicator: Nothing
  }
