module ECharts.Legend where

import Prelude
import Data.Maybe
import Data.StrMap hiding (toList)
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.List (toList)

import ECharts.Color
import ECharts.Common
import ECharts.Coords
import ECharts.Style.Text
import ECharts.Formatter

type LegendItemRec = {
  icon :: Maybe String,
  textStyle :: Maybe TextStyle
  }

data LegendItem = LegendItem String LegendItemRec

instance legendItemEncodeJson :: EncodeJson LegendItem where
  encodeJson (LegendItem name obj) =
    fromObject $ fromList $ toList
    [
      "name" := name,
      "icon" := obj.icon,
      "textStyle" := obj.textStyle
    ]

instance legendItemDecodeJson :: DecodeJson LegendItem where
  decodeJson j = do
    o <- decodeJson j
    name <- (o .? "name")
    r <- {icon: _, textStyle: _} <$> (o .? "icon") <*> (o .? "textStyle")
    pure $ LegendItem name r

legendItemDefault :: String -> LegendItem
legendItemDefault name = LegendItem name {icon: Nothing, textStyle: Nothing}

type LegendRec = {
    show :: Maybe Boolean,
    orient :: Maybe Orient,
    x :: Maybe XPos,
    y :: Maybe YPos,
    backgroundColor :: Maybe Color,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number,
    padding :: Maybe (Corner Number),
    itemGap :: Maybe Number,
    itemHeight :: Maybe Number,
    itemWidth :: Maybe Number,
    textStyle :: Maybe TextStyle,
    formatter :: Maybe Formatter,
    selectedMode :: Maybe SelectedMode,
    selected :: Maybe (StrMap Boolean),
    "data" :: Maybe (Array LegendItem)
    }


newtype Legend = Legend LegendRec
legendDefault :: LegendRec
legendDefault = {
  show: Nothing,
  orient: Nothing,
  x: Nothing,
  y: Nothing,
  backgroundColor: Nothing,
  borderColor: Nothing,
  borderWidth: Nothing,
  padding: Nothing,
  itemGap: Nothing,
  itemHeight: Nothing,
  itemWidth: Nothing,
  textStyle: Nothing,
  formatter: Nothing,
  selectedMode: Nothing,
  selected: Nothing,
  "data": Nothing
  }

instance legendEncodeJson :: EncodeJson Legend where
  encodeJson (Legend obj) =
    fromObject $ fromList $ toList
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
      "itemHeight" := obj.itemHeight,
      "itemWidth" := obj.itemWidth,
      "textStyle" := obj.textStyle,
      "formatter" := obj.formatter,
      "selectedMode" := obj.selectedMode,
      "selected" := obj.selected,
      "data" := obj."data"
    ]

instance legendDecodeJson :: DecodeJson Legend where
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
         , itemHeight: _
         , itemWidth: _
         , textStyle: _
         , formatter: _
         , selectedMode: _
         , selected: _
         , "data": _ } <$>
         (o .? "show") <*>
         (o .? "orient") <*>
         (o .? "x") <*>
         (o .? "y") <*>
         (o .? "backgroundColor") <*>
         (o .? "borderColor") <*>
         (o .? "borderWidth") <*>
         (o .? "padding") <*>
         (o .? "itemGap") <*>
         (o .? "itemHeight") <*>
         (o .? "itemWidth") <*>
         (o .? "textStyle") <*>
         (o .? "formatter") <*>
         (o .? "selectedMode") <*>
         (o .? "selected") <*>
         (o .? "data")
    pure $ Legend r

