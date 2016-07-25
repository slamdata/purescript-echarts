module ECharts.Timeline where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Axis (AxisLabel)
import ECharts.Color (Color)
import ECharts.Common (Corner, PercentOrPixel)
import ECharts.Style.Checkpoint (CheckpointStyle)
import ECharts.Style.Item (ItemStyle)
import ECharts.Style.Line (LineStyle)
import ECharts.Symbol (Symbol, SymbolSize)

data TimelineType
  = TimelineTime
  | TimelineNumber

instance timelineTypeEncodeJson ∷ EncodeJson TimelineType where
  encodeJson a = encodeJson $ case a of
    TimelineTime → "time"
    TimelineNumber → "number"

instance timelineTypeDecodeJson ∷ DecodeJson TimelineType where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "time" → pure TimelineTime
      "number" → pure TimelineNumber
      _ → Left "incorrect timeline"

data TimelineControlPosition
  = TCPLeft
  | TCPRight
  | TCPNone

instance timelineControlPositionEncodeJson ∷ EncodeJson TimelineControlPosition where
  encodeJson a = encodeJson $ case a of
    TCPNone → "none"
    TCPRight → "right"
    TCPLeft → "left"

instance timelineControlPositionDecodeJson ∷ DecodeJson TimelineControlPosition where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "none" → pure TCPNone
      "right" → pure TCPRight
      "left" → pure TCPLeft
      _ → Left "incorrect timeline control position"


type TimelineRec =
  { show ∷ Maybe Boolean
  , "type" ∷ Maybe TimelineType
  , notMerge ∷ Maybe Boolean
  , realtime ∷ Maybe Boolean
  , x ∷ Maybe PercentOrPixel
  , x2 ∷ Maybe PercentOrPixel
  , y ∷ Maybe PercentOrPixel
  , y2 ∷ Maybe PercentOrPixel
  , width ∷ Maybe PercentOrPixel
  , height ∷ Maybe PercentOrPixel
  , backgroundColor ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , borderColor ∷ Maybe Color
  , padding ∷ Maybe (Corner Number)
  , controlPosition ∷ Maybe TimelineControlPosition
  , autoPlay ∷ Maybe Boolean
  , loop ∷ Maybe Boolean
  , playInterval ∷ Maybe Number
  , lineStyle ∷ Maybe LineStyle
  , label ∷ Maybe AxisLabel
  , checkpointStyle ∷ Maybe CheckpointStyle
  , controlStyle ∷ Maybe ItemStyle
  , symbol ∷ Maybe Symbol
  , symbolSize ∷ Maybe SymbolSize
  , currentIndex ∷ Maybe Number
  , "data" ∷Maybe (Array String)
  }

newtype Timeline
  = Timeline TimelineRec

instance timelineEncodeJson ∷ EncodeJson Timeline where
  encodeJson (Timeline obj) =
    encodeJson
      $ SM.fromFoldable
          [ "show" := obj.show
          , "type" := obj."type"
          , "notMerge" := obj.notMerge
          , "realtime" := obj.realtime
          , "x" := obj.x
          , "x2" := obj.x2
          , "y" := obj.y
          , "y2" := obj.y2
          , "width" := obj.width
          , "height" := obj.height
          , "backgroundColor" := obj.backgroundColor
          , "borderWidth" := obj.borderWidth
          , "borderColor" := obj.borderColor
          , "padding" := obj.padding
          , "controlPosition" := obj.controlPosition
          , "autoPlay" := obj.autoPlay
          , "loop" := obj.loop
          , "playInterval" := obj.playInterval
          , "lineStyle" := obj.lineStyle
          , "label" := obj.label
          , "checkpointStyle" := obj.checkpointStyle
          , "controlStyle" := obj.controlStyle
          , "symbol" := obj.symbol
          , "symbolSize" := obj.symbolSize
          , "currentIndex" := obj.currentIndex
          , "data" := obj."data"
          ]

instance timelineDecodeJson ∷ DecodeJson Timeline where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , "type": _
        , notMerge: _
        , realtime: _
        , x: _
        , x2: _
        , y: _
        , y2: _
        , width: _
        , height: _
        , backgroundColor: _
        , borderWidth: _
        , borderColor: _
        , padding: _
        , controlPosition: _
        , autoPlay: _
        , loop: _
        , playInterval: _
        , lineStyle: _
        , label: _
        , checkpointStyle: _
        , controlStyle:  _
        , symbol: _
        , symbolSize: _
        , currentIndex: _
        , "data": _ }
        <$> (o .? "show")
        <*> (o .? "type")
        <*> (o .? "notMerge")
        <*> (o .? "realtime")
        <*> (o .? "x")
        <*> (o .? "x2")
        <*> (o .? "y")
        <*> (o .? "y2")
        <*> (o .? "width")
        <*> (o .? "height")
        <*> (o .? "backgroundColor")
        <*> (o .? "borderWidth")
        <*> (o .? "borderColor")
        <*> (o .? "padding")
        <*> (o .? "controlPosition")
        <*> (o .? "autoPlay")
        <*> (o .? "loop")
        <*> (o .? "playInterval")
        <*> (o .? "lineStyle")
        <*> (o .? "label")
        <*> (o .? "checkpointStyle")
        <*> (o .? "controlStyle")
        <*> (o .? "symbol")
        <*> (o .? "symbolSize")
        <*> (o .? "currentIndex")
        <*> (o .? "data")
    pure $ Timeline r


timelineDefault ∷ TimelineRec
timelineDefault =
  { show: Nothing
  , "type": Nothing
  , notMerge: Nothing
  , realtime: Nothing
  , x: Nothing
  , y: Nothing
  , x2: Nothing
  , y2: Nothing
  , width: Nothing
  , height: Nothing
  , backgroundColor: Nothing
  , borderWidth: Nothing
  , borderColor: Nothing
  , padding: Nothing
  , controlPosition: Nothing
  , autoPlay: Nothing
  , loop: Nothing
  , playInterval: Nothing
  , lineStyle: Nothing
  , label: Nothing
  , checkpointStyle: Nothing
  , controlStyle: Nothing
  , symbol: Nothing
  , symbolSize: Nothing
  , currentIndex: Nothing
  , "data": Nothing
  }
