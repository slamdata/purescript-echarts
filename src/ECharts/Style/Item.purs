module ECharts.Style.Item where

import Prelude
import Data.Maybe
import Data.List (toList)
import Data.StrMap (fromList)
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators

import ECharts.Common
import ECharts.Color
import ECharts.Coords
import ECharts.Formatter
import ECharts.Style.Line
import ECharts.Style.Area
import ECharts.Style.Chord
import ECharts.Style.Node
import ECharts.Style.Link
import ECharts.Style.Text

type ItemLabelRec = {
    show :: Maybe Boolean,
    position :: Maybe LabelPosition,
    rotate :: Maybe Boolean,
    distance :: Maybe Boolean,
    formatter :: Maybe Formatter,
    textStyle :: Maybe TextStyle
  }

newtype ItemLabel = ItemLabel ItemLabelRec


instance itemLabelEncodeJson :: EncodeJson ItemLabel where
  encodeJson (ItemLabel il) =
    fromObject $ fromList $ toList $
    [
      "show" := il.show,
      "position" := il.position,
      "distance" := il.distance,
      "formatter" := il.formatter,
      "textStyle" := il.textStyle,
      "rotate" := il.rotate
    ]

instance itemLabelDecodeJson :: DecodeJson ItemLabel where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , position: _
         , distance: _
         , formatter: _
         , textStyle: _
         , rotate: _} <$>
         (o .? "show") <*>
         (o .? "position") <*>
         (o .? "distance") <*>
         (o .? "formatter") <*>
         (o .? "textStyle") <*>
         (o .? "rotate")
    pure $ ItemLabel r


itemLabelDefault :: ItemLabelRec
itemLabelDefault = {
  show: Nothing,
  position: Nothing,
  distance: Nothing,
  formatter: Nothing,
  textStyle: Nothing,
  rotate: Nothing
  }

type ItemLabelLineRec = {
    show :: Maybe Boolean,
    length :: Maybe Number,
    lineStyle :: Maybe LineStyle
  }

newtype ItemLabelLine = ItemLabelLine ItemLabelLineRec


instance itemLabelLineEncodeJson :: EncodeJson ItemLabelLine where
  encodeJson (ItemLabelLine ill) =
    fromObject $ fromList $ toList $
    [
      "show" := ill.show,
      "length" := ill.length,
      "lineStyle" := ill.lineStyle
    ]

instance itemLabelLineDecodeJson :: DecodeJson ItemLabelLine where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , length: _
         , lineStyle: _ } <$>
         (o .? "show") <*>
         (o .? "length") <*>
         (o .? "lineStyle")
    pure $ ItemLabelLine r

itemLabelLineDefault :: ItemLabelLineRec
itemLabelLineDefault = {
  show: Nothing,
  length: Nothing,
  lineStyle: Nothing
  }


type IStyleRec = {
    color :: Maybe CalculableColor,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number,
    barBorderColor :: Maybe Color,
    barBorderRadius :: Maybe (Corner Number),
    barBorderWidth :: Maybe Number,
    label :: Maybe ItemLabel,
    labelLine :: Maybe ItemLabelLine,
    lineStyle :: Maybe LineStyle,
    areaStyle :: Maybe AreaStyle,
    chordStyle :: Maybe ChordStyle,
    nodeStyle :: Maybe NodeStyle,
    linkStyle :: Maybe LinkStyle
  }

newtype IStyle = IStyle IStyleRec

istyleDefault :: IStyleRec
istyleDefault =
  {
    color: Nothing,
    borderColor: Nothing,
    borderWidth: Nothing,
    barBorderColor: Nothing,
    barBorderRadius: Nothing,
    barBorderWidth: Nothing,
    label: Nothing,
    labelLine: Nothing,
    lineStyle: Nothing,
    areaStyle: Nothing,
    chordStyle: Nothing,
    nodeStyle: Nothing,
    linkStyle: Nothing
  }

instance istyleEncodeJson :: EncodeJson IStyle where
  encodeJson (IStyle is) =
    fromObject $ fromList $ toList
    [
      "color" := is.color,
      "borderColor" := is.borderColor,
      "borderWidth" := is.borderWidth,
      "barBorderColor" := is.barBorderColor,
      "barBorderWidth" := is.barBorderWidth,
      "barBorderRadius" := is.barBorderRadius,
      "label" := is.label,
      "labelLine" := is.labelLine,
      "lineStyle" := is.lineStyle,
      "areaStyle" := is.areaStyle,
      "chordStyle" := is.chordStyle,
      "nodeStyle" := is.nodeStyle,
      "linkStyle" := is.linkStyle
    ]

instance istyleDecodeJson :: DecodeJson IStyle where
  decodeJson j = do
    o <- decodeJson j
    r <- { color: _
         , borderColor: _
         , borderWidth: _
         , barBorderColor: _
         , barBorderWidth: _
         , barBorderRadius: _
         , label: _
         , labelLine: _
         , lineStyle: _
         , areaStyle: _
         , chordStyle: _
         , nodeStyle: _
         , linkStyle: _ } <$>
         (o .? "color") <*>
         (o .? "borderColor" ) <*>
         (o .? "borderWidth") <*>
         (o .? "barBorderColor") <*>
         (o .? "barBorderWidth") <*>
         (o .? "barBorderRadius") <*>
         (o .? "label") <*>
         (o .? "labelLine") <*>
         (o .? "lineStyle") <*>
         (o .? "areaStyle") <*>
         (o .? "chordStyle") <*>
         (o .? "nodeStyle") <*>
         (o .? "linkStyle")
    pure $ IStyle r

type ItemStyleRec = {
    normal :: Maybe IStyle,
    emphasis :: Maybe IStyle
  }

newtype ItemStyle = ItemStyle ItemStyleRec



instance itemStyleEncodeJson :: EncodeJson ItemStyle where
  encodeJson (ItemStyle is) =
    fromObject $ fromList $ toList ["normal" := is.normal, "emphasis" := is.emphasis]

instance itemStyleDecodeJson :: DecodeJson ItemStyle where
  decodeJson j = do
    o <- decodeJson j
    r <- {normal: _, emphasis: _} <$> (o .? "normal") <*> (o .? "emphasis")
    pure $ ItemStyle r

itemStyleDefault :: ItemStyleRec
itemStyleDefault = {
  normal: Nothing,
  emphasis: Nothing
  }
