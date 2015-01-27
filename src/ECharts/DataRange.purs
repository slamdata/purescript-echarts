module ECharts.DataRange where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.StrMap (fromList)
import Data.Tuple

import ECharts.Color
import ECharts.Coords
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Text

newtype DataRange =
  DataRange {
    "show" :: Maybe Boolean,
    "orient" :: Maybe Orient,
    "x" :: Maybe XPos,
    "y" :: Maybe YPos,
    "backgroundColor" :: Maybe Color,
    "borderColor" :: Maybe Color,
    "borderWidth" :: Maybe Number,
    "padding" :: Maybe (Corner Number),
    "itemGap" :: Maybe Number,
    "itemWidth" :: Maybe Number,
    "itemHeight" :: Maybe Number,
    "min" :: Maybe Number,
    "max" :: Maybe Number,
    "precision" :: Maybe Number,
    "splitNumber" :: Maybe Number,
    "selectedMode" :: Maybe SelectedMode,
    "calculable" :: Maybe Boolean,
    "hoverLink" :: Maybe Boolean,
    "realtime" :: Maybe Boolean,
    "color" :: Maybe [Color],
    "formatter" :: Maybe Formatter,
    "text" :: Maybe (Tuple String String),
    "textStyle" :: Maybe TextStyle
    }

instance dataRangeEncodeJson :: EncodeJson DataRange where
  encodeJson (DataRange obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "orient" := obj.orient,
      "x" := obj.x,
      "y" := obj.y,
      "backgroundColor" := obj.backgroundColor,
      "borderColor" := obj.borderColor,
      "borderWidth" := obj.borderWidth,
      "padding" := obj.padding,
      "itemGap" := obj.itemGap,
      "itemWidth" := obj.itemWidth,
      "itemHeight" := obj.itemHeight,
      "min" := obj.min,
      "max" := obj.max,
      "precision" := obj.precision,
      "splitNumber" := obj.splitNumber,
      "selectedMode" := obj.selectedMode,
      "calculable" := obj.calculable,
      "hoverLink" := obj.hoverLink,
      "realtime" := obj.realtime,
      "color" := obj.color,
      "formatter" := obj.formatter,
      "text" := obj.text,
      "textStyle" := obj.textStyle
    ]

dataRangeDefault = {
  show: Nothing,
  orient: Nothing,
  x: Nothing,
  y: Nothing,
  backgroundColor: Nothing,
  borderColor: Nothing,
  borderWidth: Nothing,
  padding: Nothing,
  itemGap: Nothing,
  itemWidth: Nothing,
  itemHeight: Nothing,
  min: Nothing,
  max: Nothing,
  precision: Nothing,
  splitNumber: Nothing,
  selectedMode: Nothing,
  calculable: Nothing,
  hoverLink: Nothing,
  realtime: Nothing,
  formatter: Nothing,
  text: Nothing,
  textStyle: Nothing
  }
