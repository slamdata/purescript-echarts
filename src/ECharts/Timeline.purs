module ECharts.Timeline where

import Data.Maybe
import Data.StrMap
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import ECharts.Style.Item
import ECharts.Style.Checkpoint
import ECharts.Style.Line
import ECharts.Symbol
import ECharts.Color
import ECharts.Axis
import ECharts.Common

data TimelineType = TimelineTime | TimelineNumber

instance timelineTypeEncodeJson :: EncodeJson TimelineType where
  encodeJson a = encodeJson $ case a of
    TimelineTime -> "time"
    TimelineNumber -> "number"

data TimelineControlPosition = TCPLeft | TCPRight | TCPNone

instance timelineControlPositionEncodeJson :: EncodeJson TimelineControlPosition where
  encodeJson a = encodeJson $ case a of
    TCPNone -> "none"
    TCPRight -> "right"
    TCPLeft -> "left"
                                      

newtype Timeline =
  Timeline {
    "show" :: Maybe Boolean,
    "type" :: Maybe TimelineType,
    "notMerge" :: Maybe Boolean,
    "realtime" :: Maybe Boolean,
    "x" :: Maybe PercentOrPixel,
    "x2" :: Maybe PercentOrPixel,
    "y" :: Maybe PercentOrPixel,
    "y2" :: Maybe PercentOrPixel,
    "width" :: Maybe PercentOrPixel,
    "height" :: Maybe PercentOrPixel,
    "backgroundColor" :: Maybe Color,
    "borderWidth" :: Maybe Number,
    "borderColor" :: Maybe Color,
    "padding" :: Maybe (Corner Number) ,
    "controlPosition" :: Maybe TimelineControlPosition,
    "autoPlay" :: Maybe Boolean,
    "loop" :: Maybe Boolean,
    "playInterval" :: Maybe Number,
    "lineStyle" :: Maybe LineStyle,
    "label" :: Maybe AxisLabel,
    "checkpointStyle" :: Maybe CheckpointStyle ,
    "controlStyle" :: Maybe ItemStyle,
    "symbol" :: Maybe Symbol,
    "symbolSize" :: Maybe SymbolSize,
    "currentIndex" :: Maybe Number,
    "data" :: Maybe [String]
    }

instance timelineEncodeJson :: EncodeJson Timeline where
  encodeJson (Timeline obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "type" := obj.type,
      "notMerge" := obj.notMerge,
      "realtime" := obj.realtime,
      "x" := obj.x,
      "x2" := obj.x2,
      "y" := obj.y,
      "y2" := obj.y2,
      "width" := obj.width,
      "height" := obj.height,
      "backgroundColor" := obj.backgroundColor,
      "borderWidth" := obj.borderWidth,
      "borderColor" := obj.borderColor,
      "padding" := obj.padding,
      "controlPosition" := obj.controlPosition,
      "autoPlay" := obj.autoPlay,
      "loop" := obj.loop,
      "playInterval" := obj.playInterval,
      "lineStyle" := obj.lineStyle,
      "label" := obj.label,
      "checkpointStyle" := obj.checkpointStyle,
      "controlStyle" := obj.controlStyle,
      "symbol" := obj.symbol,
      "symbolSize" := obj.symbolSize,
      "currentIndex" := obj.currentIndex,
      "data" := obj.data
    ]
