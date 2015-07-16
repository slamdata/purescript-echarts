module ECharts.Mark.Point (
  MarkPoint(..),
  MarkPointRec(),
  markPointDefault,
  addMarkPoint,
  delMarkPoint
  ) where

import Prelude
import ECharts.Chart
import ECharts.Common
import ECharts.Symbol
import ECharts.Mark.Effect
import ECharts.Mark.Data
import ECharts.Effects

import Data.Maybe
import Control.Monad.Eff
import Data.Function

import Data.StrMap (fromList, StrMap (..))
import Data.List (toList)
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators

type MarkPointRec = {
    symbol :: Maybe Symbol,
    symbolSize :: Maybe SymbolSize,
    large :: Maybe Boolean,
    effect :: Maybe MarkPointEffect,
    "data" :: Maybe (Array MarkPointData),
    geoCoord:: Maybe (StrMap (Tuple Number Number))
  }

newtype MarkPoint = MarkPoint MarkPointRec
   

instance markPointEncodeJson :: EncodeJson MarkPoint where
  encodeJson (MarkPoint mp) =
    fromObject $ fromList $ toList
    [
      "symbol" := mp.symbol,
      "symbolSize" := mp.symbolSize,
      "large" := mp.large,
      "effect" := mp.effect,
      "data" := mp.data,
      "geoCoord" := mp.geoCoord
    ]

instance markPointDecodeJson :: DecodeJson MarkPoint where
  decodeJson j = do
    o <- decodeJson j
    r <- { symbol: _
         , symbolSize: _
         , large: _
         , effect: _
         , "data": _
         , geoCoord: _ } <$>
         (o .? "symbol") <*>
         (o .? "symbolSize") <*>
         (o .? "large") <*>
         (o .? "effect") <*>
         (o .? "data") <*>
         (o .? "geoCoord")
    pure $ MarkPoint r

markPointDefault :: MarkPointRec
markPointDefault =
  {
    symbol: Nothing,
    symbolSize: Nothing,
    large: Nothing,
    effect: Nothing,
    "data": Nothing,
    geoCoord: Nothing
  }

foreign import delMarkPointImpl :: forall e. Fn3 Number String EChart
       (Eff (removeMarkPointECharts::REMOVE_MARKPOINT|e) EChart)

delMarkPoint :: forall e. Number -> String -> EChart -> 
                (Eff (removeMarkPointECharts::REMOVE_MARKPOINT|e) EChart)
delMarkPoint idx name chart = runFn3 delMarkPointImpl idx name chart 
  
foreign import addMarkPointImpl :: forall e. Fn2 Json EChart (Eff (addMarkPointECharts::ADD_MARKPOINT|e) EChart)

addMarkPoint :: forall e. MarkPoint -> EChart -> 
                (Eff (addMarkPointECharts::ADD_MARKPOINT|e) EChart)
addMarkPoint mp chart = runFn2 addMarkPointImpl (encodeJson mp) chart
