module ECharts.Series.Force where

import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap

import ECharts.Common
import ECharts.Coords
import ECharts.Tooltip
import ECharts.Type
import ECharts.Style.Item
import ECharts.Mark.Line
import ECharts.Mark.Point
import ECharts.Item.Data
import ECharts.Symbol
import ECharts.Series.Force

newtype ForceCategory =
  ForceCategory {
    "name" :: Maybe String,
    "symbol" :: Maybe Symbol,
    "symbolSize" :: Maybe SymbolSize,
    "itemStyle" :: Maybe ItemStyle
    }

forceCategoryDefault = {
  name: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  itemStyle: Nothing
  }

instance forceCategoryEncodeJson :: EncodeJson ForceCategory where
   encodeJson (ForceCategory fc) = fromObject $ fromList $ [
     "name" := fc.name,
     "symbol" := fc.symbol,
     "symbolSize" := fc.symbolSize,
     "itemStyle" := fc.itemStyle
     ]

newtype Node =
  Node {
    "name" :: Maybe String,
    "label" :: Maybe String,
    "value" :: Number,
    "ignore" :: Maybe Boolean,
    "symbol" :: Maybe Symbol,
    "symbolSize" :: Maybe SymbolSize,
    "itemStyle" :: Maybe ItemStyle,

    "initial" :: Maybe (Tuple Number Number),
    "fixX" :: Maybe Boolean,
    "fixY" :: Maybe Boolean,
    "draggable" :: Maybe Boolean,
    "category" :: Maybe Number
    }

nodeDefault value = {
  name: Nothing,
  label: Nothing,
  value: value,
  ignore: Nothing,
  symbol: Nothing,
  symbolSize: Nothing,
  itemStyle: Nothing,
  initial: Nothing,
  fixX: Nothing,
  fixY: Nothing,
  draggable: Nothing,
  category: Nothing
  }

instance nodeEncodeJson :: EncodeJson Node where
  encodeJson (Node n) = fromObject $ fromList $ [
    "name" := n.name,
    "label" := n.label,
    "value" := n.value,
    "ignore" := n.ignore,
    "symbol" := n.symbol,
    "symbolSize" := n.symbolSize,
    "itemStyle" := n.itemStyle,
    "initial" := n.initial,
    "fixX" := n.fixX,
    "fixY" := n.fixY,
    "draggable" := n.draggable,
    "category" := n.category
    ]

data LinkEnd = Name String | Index Number

instance linkEndEncodeJson :: EncodeJson LinkEnd where
  encodeJson (Name name) = encodeJson name
  encodeJson (Index id) = encodeJson id 

newtype Link =
  Link {
    source :: LinkEnd,
    target :: LinkEnd,
    weight :: Number,
    itemStyle :: Maybe ItemStyle 
    }

instance linkEncodeJson :: EncodeJson Link where
  encodeJson (Link link) = fromObject $ fromList $ [
    "source" := link.source,
    "target" := link.target,
    "weight" := link.weight,
    "itemStyle" := link.itemStyle
    ]

type Matrix = [[Number]]

