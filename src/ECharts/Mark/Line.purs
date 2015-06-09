module ECharts.Mark.Line (
  MarkLine(..),
  MarkLineRec(),
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
import Data.Argonaut.Decode
import Data.Argonaut.Combinators

type MarkLineRec = {
    symbol :: Maybe (Tuple Symbol Symbol),
    symbolSize :: Maybe DoubleSymbolSize,
    symbolRotate :: Maybe (Tuple Number Number),
    effect :: Maybe MarkPointEffect,
    geoCoord :: Maybe [GeoCoord],
    "data" :: Maybe [Tuple MarkPointData MarkPointData],
    itemStyle :: Maybe ItemStyle
  }

newtype MarkLine = MarkLine MarkLineRec
   

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

instance mlDecodeJson :: DecodeJson MarkLine where
  decodeJson j = do
    o <- decodeJson j
    r <- { symbol: _
         , symbolSize: _
         , symbolRotate: _
         , effect: _
         , geoCoord: _
         , "data": _
         , itemStyle: _ } <$>
         (o .? "symbol") <*>
         (o .? "symbolSize") <*>
         (o .? "symbolRotate") <*>
         (o .? "effect") <*>
         (o .? "geoCoord") <*>
         (o .? "data") <*>
         (o .? "itemStyle")
    pure $ MarkLine r
           

markLineDefault :: MarkLineRec
markLineDefault =
  {
    symbol: Nothing,
    symbolSize: Nothing,
    symbolRotate: Nothing,
    effect: Nothing,
    geoCoord: Nothing,
    "data": Nothing,
    itemStyle: Nothing
  }


foreign import addMarkLineImpl """
function addMarkLineImpl(ml, chart) {
  return function() {
    return chart.addMarkLine(ml);
  };
}
""" :: forall e. Fn2 Json EChart (Eff (addMarkLineECharts::ADD_MARKLINE|e) EChart)


addMarkLine :: forall e a. MarkLine  -> EChart -> 
               Eff (addMarkLineECharts::ADD_MARKLINE|e) EChart
addMarkLine ml chart = runFn2 addMarkLineImpl (encodeJson ml) chart




foreign import delMarkLineImpl """
function delMarkLineImpl(idx, name, chart) {
  return function() {
    return chart.delMarkLine(idx, name);
  };
}
""" :: forall e. Fn3 Number String EChart
       (Eff (removeMarkLine::REMOVE_MARKLINE|e) EChart)

delMarkLine :: forall e. Number -> String -> EChart -> 
               Eff (removeMarkLine::REMOVE_MARKLINE|e) EChart
delMarkLine idx name chart = runFn3 delMarkLineImpl idx name chart

