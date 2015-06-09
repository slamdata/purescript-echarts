module ECharts.Tooltip (
  TooltipTrigger(..),
  TooltipPosition(..),
  TooltipAxisPointerType(..),
  TooltipAxisPointer(..),
  TooltipAxisPointerRec(),
  Tooltip(..),
  TooltipRec(),
  
  tooltipAxisPointerDefault,
  tooltipDefault
  ) where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.Either
import Data.StrMap
import Data.Function

import ECharts.Common
import ECharts.Coords
import ECharts.Color
import ECharts.Style.Text
import ECharts.Style.Line
import ECharts.Style.Area
import ECharts.Formatter
import ECharts.Utils

data TooltipTrigger = TriggerItem | TriggerAxis
instance tooltipTriggerEncodeJson :: EncodeJson TooltipTrigger where
  encodeJson TriggerItem = encodeJson "item"
  encodeJson TriggerAxis = encodeJson "axis"

instance tooltipTriggerDecodeJson :: DecodeJson TooltipTrigger where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "item" -> pure TriggerItem
      "axis" -> pure TriggerAxis
      _ -> Left $ "incorrect tooltip trigger"



foreign import func2json """
function func2json(fn) {
  return fn;
}
""" :: forall a. a -> Json

data TooltipPosition = Fixed [Number] | FuncPos ([Number] -> [Number])
instance tooltipPositionEncodeJson :: EncodeJson TooltipPosition where
  encodeJson (Fixed nums) = encodeJson nums
  encodeJson (FuncPos func) = func2json $ mkFn1 func

instance tooltipPositionDecodeJson :: DecodeJson TooltipPosition where
  decodeJson nums = Fixed <$> decodeJson nums


data TooltipAxisPointerType =
  LinePointer | CrossPointer | ShadowPointer | NonePointer
instance tooltipAxisPointerTypeEncodeJson :: EncodeJson TooltipAxisPointerType where
  encodeJson a = encodeJson $ case a of
    LinePointer -> "line"
    CrossPointer -> "cross"
    ShadowPointer -> "shadow"
    NonePointer -> "none"

instance tooltiopAxisPointerTypeDecodeJson :: DecodeJson TooltipAxisPointerType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "line" -> pure LinePointer
      "cross" -> pure CrossPointer
      "shadow" -> pure ShadowPointer
      "none" -> pure NonePointer
      _ -> Left "incorrect tooltip axis pointer type"

type TooltipAxisPointerRec = {
    "type" :: Maybe TooltipAxisPointerType,
    lineStyle :: Maybe LineStyle,
    crossStyle :: Maybe LineStyle,
    shadowStyle :: Maybe AreaStyle
    }

newtype TooltipAxisPointer = TooltipAxisPointer TooltipAxisPointerRec

instance tooltipAxisPointerEncodeJson :: EncodeJson TooltipAxisPointer where
  encodeJson (TooltipAxisPointer obj) =
    fromObject $ fromList $
    [
      "type" := obj.type,
      "lineStyle" := obj.lineStyle,
      "crossStyle" := obj.crossStyle,
      "shadowStyle" := obj.shadowStyle
    ]

instance tooltipAxisPointerDecodeJson :: DecodeJson TooltipAxisPointer where
  decodeJson json = do
    o <- decodeJson json
    r <- { "type": _
         , lineStyle: _
         , crossStyle: _
         , shadowStyle: _ } <$>
         (o .? "type") <*>
         (o .? "lineStyle") <*>
         (o .? "crossStyle") <*>
         (o .? "shadowStyle")
    pure $ TooltipAxisPointer r
tooltipAxisPointerDefault :: TooltipAxisPointerRec
tooltipAxisPointerDefault = {
  "type": Nothing,
  lineStyle: Nothing,
  crossStyle: Nothing,
  shadowStyle: Nothing
  }

type TooltipRec = {
    show :: Maybe Boolean,
    showContent :: Maybe Boolean,
    trigger :: Maybe TooltipTrigger,
    position :: Maybe TooltipPosition,
    formatter :: Maybe Formatter,
    islandFormatter :: Maybe Formatter,
    showDelay :: Maybe Number,
    hideDelay :: Maybe Number,
    transitionDuration :: Maybe Number,
    backgroundColor :: Maybe Color,
    borderColor :: Maybe Color,
    borderRadius :: Maybe Number,
    borderWidth :: Maybe Number,
    padding :: Maybe (Corner Number),
    axisPointer :: Maybe TooltipAxisPointer,
    textStyle :: Maybe TextStyle,
    enterable :: Maybe Boolean
    }


newtype Tooltip = Tooltip TooltipRec


instance tooltipEncodeJson :: EncodeJson Tooltip where
  encodeJson (Tooltip obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "showContent" := obj.showContent,
      "trigger" := obj.trigger,
      "position" := obj.position,
      "formatter" := obj.formatter,
      "islandFormatter" := obj.islandFormatter,
      "showDelay" := obj.showDelay,
      "hideDelay" := obj.hideDelay,
      "transitionDuration" := obj.transitionDuration,
      "backgroundColor" := obj.backgroundColor,
      "borderColor" := obj.borderColor,
      "borderRadius" := obj.borderRadius,
      "borderWidth" := obj.borderWidth,
      "padding" := obj.padding,
      "axisPointer" := obj.axisPointer,
      "textStyle" := obj.textStyle,
      "enterable" := obj.enterable
    ]


instance tooltipDecodeJson :: DecodeJson Tooltip where
  decodeJson json = do
    o <- decodeJson json
    r <- { show: _
         , showContent: _
         , trigger: _
         , position: _
         , formatter: _
         , islandFormatter: _
         , showDelay: _
         , hideDelay: _
         , transitionDuration: _
         , backgroundColor: _
         , borderColor: _
         , borderRadius: _
         , borderWidth: _
         , padding: _
         , axisPointer: _
         , textStyle: _
         , enterable: _ } <$>
         (o .? "show") <*>
         (o .? "showContent") <*>
         (o .? "trigger") <*>
         (o .? "position") <*>
         (o .? "formatter") <*>
         (o .? "islandFormatter") <*>
         (o .? "showDelay") <*>
         (o .? "hideDelay") <*>
         (o .? "transitionDuration") <*>
         (o .? "backgroundColor") <*>
         (o .? "borderColor") <*>
         (o .? "borderRadius") <*>
         (o .? "borderWidth") <*>
         (o .? "padding") <*>
         (o .? "axisPointer") <*>
         (o .? "textStyle") <*>
         (o .? "enterable")
    pure $ Tooltip r
         

tooltipDefault :: TooltipRec
tooltipDefault = {
  show: Nothing,
  showContent: Nothing,
  trigger: Nothing,
  position: Nothing,
  formatter: Nothing,
  islandFormatter: Nothing,
  showDelay: Nothing,
  hideDelay: Nothing,
  transitionDuration: Nothing,
  backgroundColor: Nothing,
  borderColor: Nothing,
  borderRadius: Nothing,
  borderWidth: Nothing,
  padding: Nothing,
  axisPointer: Nothing,
  textStyle: Nothing,
  enterable: Nothing
  }
