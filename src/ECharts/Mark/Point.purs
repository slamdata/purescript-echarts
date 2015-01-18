module ECharts.Mark.Point where

import ECharts.Chart
import ECharts.Common
import ECharts.Symbol
import ECharts.Mark.Effect
import ECharts.Mark.Data

import Data.Maybe
import Control.Monad.Eff
import Data.Function

import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators





newtype MarkPoint =
  MarkPoint {
    symbol :: Maybe Symbol,
    symbolSize :: Maybe SymbolSize,
    large :: Maybe Boolean,
    effect :: Maybe MarkPointEffect,
    "data" :: Maybe [MarkPointData],
    geoCoord:: Maybe [GeoCoord]
  }

instance markPointEncodeJson :: EncodeJson MarkPoint where
  encodeJson (MarkPoint mp) =
    fromObject $ fromList $
    [
      "symbol" := mp.symbol,
      "symbolSize" := mp.symbolSize,
      "large" := mp.large,
      "effect" := mp.effect,
      "data" := mp.data,
      "geoCoord" := mp.geoCoord
    ]

emptyMarkPoint =
  {
    symbol: Nothing,
    symbolSize: Nothing,
    large: Nothing,
    effect: Nothing,
    "data": Nothing,
    geoCoord: Nothing
  }

foreign import delMarkPointImpl """
function delMarkPointImpl(idx, name, chart) {
  return function() {
    return chart.delMarkPoint(idx, name);
  };
}
""" :: forall e. Fn3 Number String EChart (Eff e EChart)

delMarkPoint :: forall e. Number -> String -> EChart -> Eff e EChart
delMarkPoint idx name chart = runFn3 delMarkPointImpl idx name chart 
  
foreign import addMarkPointImpl """
function addMarkPointImpl(mp, chart) {
  return function() {
    return chart.addMarkPoint(mp);
  };
}
""" :: forall e. Fn2 Json EChart (Eff e EChart)

addMarkPoint :: forall e. MarkPoint -> EChart -> Eff e EChart
addMarkPoint mp chart = runFn2 addMarkPointImpl (encodeJson mp) chart
