module ECharts.Mark.Line
  ( MarkLine(..)
  , MarkLineRec
  , markLineDefault
  , addMarkLine
  , delMarkLine
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.StrMap as SM

import ECharts.Chart (EChart)
import ECharts.Common (GeoCoord)
import ECharts.Mark.Effect (MarkPointEffect)
import ECharts.Mark.Data (MarkPointData)
import ECharts.Style.Item (ItemStyle)
import ECharts.Symbol (DoubleSymbolSize, Symbol)
import ECharts.Effects (ECHARTS)


type MarkLineRec =
  { symbol ∷ Maybe (Tuple Symbol Symbol)
  , symbolSize ∷ Maybe DoubleSymbolSize
  , symbolRotate ∷ Maybe (Tuple Number Number)
  , effect ∷ Maybe MarkPointEffect
  , geoCoord ∷ Maybe (Array GeoCoord)
  , "data" ∷ Maybe (Array (Tuple MarkPointData MarkPointData))
  , itemStyle ∷ Maybe ItemStyle
  }

newtype MarkLine
  = MarkLine MarkLineRec

instance mlEncodeJson ∷ EncodeJson MarkLine where
  encodeJson (MarkLine ml) =
    encodeJson
      $ SM.fromFoldable
        [ "symbol" := ml.symbol
        , "symbolSize" := ml.symbolSize
        , "symbolRotate" := ml.symbolRotate
        , "effect" := ml.effect
        , "geoCoord" := ml.geoCoord
        , "data" := ml."data"
        , "itemStyle" := ml.itemStyle
        ]

instance mlDecodeJson ∷ DecodeJson MarkLine where
  decodeJson j = do
    o ← decodeJson j
    r ← { symbol: _
        , symbolSize: _
        , symbolRotate: _
        , effect: _
        , geoCoord: _
        , "data": _
        , itemStyle: _ }
        <$> (o .? "symbol")
        <*> (o .? "symbolSize")
        <*> (o .? "symbolRotate")
        <*> (o .? "effect")
        <*> (o .? "geoCoord")
        <*> (o .? "data")
        <*> (o .? "itemStyle")
    pure $ MarkLine r


markLineDefault ∷ MarkLineRec
markLineDefault =
  { symbol: Nothing
  , symbolSize: Nothing
  , symbolRotate: Nothing
  , effect: Nothing
  , geoCoord: Nothing
  , "data": Nothing
  , itemStyle: Nothing
  }


foreign import addMarkLineImpl
  ∷ ∀ e. Fn2 Json EChart (Eff (echarts ∷ ECHARTS |e) EChart)


addMarkLine
 ∷ ∀ e
 . MarkLine
 → EChart
 → Eff (echarts ∷ ECHARTS |e) EChart
addMarkLine ml chart =
  runFn2 addMarkLineImpl (encodeJson ml) chart


foreign import delMarkLineImpl
  ∷ ∀ e. Fn3 Number String EChart (Eff (echarts ∷ ECHARTS |e) EChart)

delMarkLine
  ∷ ∀ e
  . Number
  → String
  → EChart
  → Eff (echarts ∷ ECHARTS |e) EChart
delMarkLine idx name chart =
  runFn3 delMarkLineImpl idx name chart
