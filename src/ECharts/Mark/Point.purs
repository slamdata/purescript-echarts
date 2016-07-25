module ECharts.Mark.Point
  ( MarkPoint(..)
  , MarkPointRec
  , markPointDefault
  , addMarkPoint
  , delMarkPoint
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.StrMap as SM

import ECharts.Chart (EChart)
import ECharts.Symbol (Symbol, SymbolSize)
import ECharts.Mark.Effect (MarkPointEffect)
import ECharts.Mark.Data (MarkPointData)
import ECharts.Effects (ECHARTS)

type MarkPointRec =
  { symbol ∷ Maybe Symbol
  , symbolSize ∷ Maybe SymbolSize
  , large ∷ Maybe Boolean
  , effect ∷ Maybe MarkPointEffect
  , "data" ∷ Maybe (Array MarkPointData)
  , geoCoord∷ Maybe (SM.StrMap (Tuple Number Number))
  }

newtype MarkPoint
  = MarkPoint MarkPointRec

instance markPointEncodeJson ∷ EncodeJson MarkPoint where
  encodeJson (MarkPoint mp) =
    encodeJson
      $ SM.fromFoldable
        [ "symbol" := mp.symbol
        , "symbolSize" := mp.symbolSize
        , "large" := mp.large
        , "effect" := mp.effect
        , "data" := mp."data"
        , "geoCoord" := mp.geoCoord
        ]

instance markPointDecodeJson ∷ DecodeJson MarkPoint where
  decodeJson j = do
    o ← decodeJson j
    r ← { symbol: _
        , symbolSize: _
        , large: _
        , effect: _
        , "data": _
        , geoCoord: _ }
        <$> (o .? "symbol")
        <*> (o .? "symbolSize")
        <*> (o .? "large")
        <*> (o .? "effect")
        <*> (o .? "data")
        <*> (o .? "geoCoord")
    pure $ MarkPoint r

markPointDefault ∷ MarkPointRec
markPointDefault =
  { symbol: Nothing
  , symbolSize: Nothing
  , large: Nothing
  , effect: Nothing
  , "data": Nothing
  , geoCoord: Nothing
  }

foreign import delMarkPointImpl
  ∷ ∀ e. Fn3 Number String EChart (Eff (echarts ∷ ECHARTS|e) EChart)

delMarkPoint
  ∷ ∀ e
  . Number
  → String
  → EChart
  → Eff (echarts ∷ ECHARTS|e) EChart
delMarkPoint idx name chart =
  runFn3 delMarkPointImpl idx name chart

foreign import addMarkPointImpl
  ∷ ∀ e. Fn2 Json EChart (Eff (echarts ∷ ECHARTS|e) EChart)

addMarkPoint
  ∷ ∀ e
  . MarkPoint
  → EChart
  → Eff (echarts ∷ ECHARTS |e) EChart
addMarkPoint mp chart =
  runFn2 addMarkPointImpl (encodeJson mp) chart
