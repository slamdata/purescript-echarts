module ECharts.Style.Item where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
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

newtype ItemLabel =
  ItemLabel {
    show :: Maybe Boolean,
    position :: Maybe LabelPosition,
    rotate :: Maybe Boolean,
    distance :: Maybe Boolean,
    formatter :: Maybe Formatter,
    textStyle :: Maybe TextStyle
  }

instance itemLabelEncodeJson :: EncodeJson ItemLabel where
  encodeJson (ItemLabel il) =
    fromObject $ fromList $
    [
      "show" := il.show,
      "position" := il.position,
      "distance" := il.distance,
      "formatter" := il.formatter,
      "textStyle" := il.textStyle
    ]
itemLabelDefault = {
  show: Nothing,
  position: Nothing,
  distance: Nothing,
  formatter: Nothing,
  textStyle: Nothing
  }

newtype ItemLabelLine =
  ItemLabelLine {
    show :: Maybe Boolean,
    length :: Maybe Number,
    lineStyle :: Maybe LineStyle
  }

instance itemLabelLineEncodeJson :: EncodeJson ItemLabelLine where
  encodeJson (ItemLabelLine ill) =
    fromObject $ fromList $
    [
      "show" := ill.show,
      "length" := ill.length,
      "lineStyle" := ill.lineStyle
    ]
itemLabelLineDefault = {
  show: Nothing,
  length: Nothing,
  lineStyle: Nothing
  }

newtype IStyle =
  IStyle {
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
    fromObject $ fromList $
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

newtype ItemStyle =
  ItemStyle {
    normal :: Maybe IStyle,
    emphasis :: Maybe IStyle
  }


instance itemStyleEncodeJson :: EncodeJson ItemStyle where
  encodeJson (ItemStyle is) =
    fromObject $ fromList ["normal" := is.normal, "emphasis" := is.emphasis]

itemStyleDefault = {
  normal: Nothing,
  emphasis: Nothing
  }
