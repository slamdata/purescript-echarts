module ECharts.DataRange where

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.StrMap (fromList)
import Data.Tuple
import Data.List (toList)

import ECharts.Color
import ECharts.Coords
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Text

type DataRangeRec = {
    show :: Maybe Boolean,
    orient :: Maybe Orient,
    x :: Maybe XPos,
    y :: Maybe YPos,
    backgroundColor :: Maybe Color,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number,
    padding :: Maybe (Corner Number),
    itemGap :: Maybe Number,
    itemWidth :: Maybe Number,
    itemHeight :: Maybe Number,
    min :: Maybe Number,
    max :: Maybe Number,
    precision :: Maybe Number,
    splitNumber :: Maybe Number,
    selectedMode :: Maybe SelectedMode,
    calculable :: Maybe Boolean,
    hoverLink :: Maybe Boolean,
    realtime :: Maybe Boolean,
    color :: Maybe (Array Color),
    formatter :: Maybe Formatter,
    text :: Maybe (Tuple String String),
    textStyle :: Maybe TextStyle
    }

newtype DataRange = DataRange DataRangeRec


instance dataRangeEncodeJson :: EncodeJson DataRange where
  encodeJson (DataRange obj) =
    fromObject $ fromList $ toList $
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

instance dataRangeDecodeJson :: DecodeJson DataRange where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , orient: _
         , x: _
         , y: _
         , backgroundColor: _
         , borderColor: _
         , borderWidth: _
         , padding: _
         , itemGap: _
         , itemWidth: _
         , itemHeight: _
         , min: _
         , max: _
         , precision: _
         , splitNumber: _
         , selectedMode: _
         , calculable: _
         , hoverLink: _
         , realtime: _
         , color: _
         , formatter: _
         , text: _
         , textStyle: _ } <$>
         (o .? "show") <*>
         (o .? "orient") <*>
         (o .? "x") <*>
         (o .? "y") <*>
         (o .? "backgroundColor") <*>
         (o .? "borderColor") <*>
         (o .? "borderWidth") <*>
         (o .? "padding") <*>
         (o .? "itemGap") <*>
         (o .? "itemWidth") <*>
         (o .? "itemHeight") <*>
         (o .? "min") <*>
         (o .? "max") <*>
         (o .? "precision") <*>
         (o .? "splitNumber") <*>
         (o .? "selectedMode") <*>
         (o .? "calculable") <*>
         (o .? "hoverLink") <*>
         (o .? "realtime") <*>
         (o .? "color") <*>
         (o .? "formatter") <*>
         (o .? "text") <*>
         (o .? "textStyle")
    pure $ DataRange r

dataRangeDefault :: DataRangeRec
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
  textStyle: Nothing,
  color: Nothing
  }
