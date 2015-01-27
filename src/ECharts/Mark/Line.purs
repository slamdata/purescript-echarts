module ECharts.Mark.Line (
  MarkLine(..),
  markLineDefault,
  addMarkLine,
  delMarkLine
  ) where


import ECharts.Chart
import ECharts.Common
import ECharts.Mark.Effect
import ECharts.Mark.Data
import ECharts.Style.Item
import ECharts.Symbol
import ECharts.Effects

import Data.Maybe
import Control.Monad.Eff
import Data.Function

import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators



newtype MarkLine =
  MarkLine {
    symbol :: Maybe (Tuple Symbol Symbol),
    symbolSize :: Maybe DoubleSymbolSize,
    symbolRotate :: Maybe (Tuple Number Number),
    effect :: Maybe MarkPointEffect,
    geoCoord :: Maybe [GeoCoord],
    "data" :: Maybe [Tuple MarkPointData MarkPointData],
    itemStyle :: Maybe ItemStyle
  }

instance mlEncodeJson :: EncodeJson MarkLine where
  encodeJson (MarkLine ml) =
    fromObject $ fromList $
    [
      "symbol" := ml.symbol,
      "symbolSize" := ml.symbolSize,
      "symbolRotate" := ml.symbolRotate,
      "effect" := ml.effect,
      "geoCoord" := ml.geoCoord,
      "data" := ml.data,
      "itemStyle" := ml.itemStyle
    ]

markLineDefault =
  {
    symbol: Nothing,
    symbolSize: Nothing,
    symbolRotate: Nothing,
    effect: Nothing,
    geoCoord: Nothing,
    "data": Nothing
  }


foreign import addMarkLineImpl """
function addMarkLineImpl(ml, chart) {
  return function() {
    return chart.addMarkLine(ml);
  };
}
""" :: forall e. Fn2 Json EChart (Eff (addMarkLineECharts::AddMarkLine|e) EChart)


addMarkLine :: forall e a. MarkLine  -> EChart -> 
               Eff (addMarkLineECharts::AddMarkLine|e) EChart
addMarkLine ml chart = runFn2 addMarkLineImpl (encodeJson ml) chart




foreign import delMarkLineImpl """
function delMarkLineImpl(idx, name, chart) {
  return function() {
    return chart.delMarkLine(idx, name);
  };
}
""" :: forall e. Fn3 Number String EChart
       (Eff (removeMarkLine::RemoveMarkLine|e) EChart)

delMarkLine :: forall e. Number -> String -> EChart -> 
               Eff (removeMarkLine::RemoveMarkLine|e) EChart
delMarkLine idx name chart = runFn3 delMarkLineImpl idx name chart

