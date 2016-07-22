module ECharts.Style.Checkpoint where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Symbol (Symbol, SymbolSize)
import ECharts.Color (Color)
import ECharts.Axis (AxisLabel)

type CheckpointStyleRec =
  { symbol ∷ Maybe Symbol
  , symbolSize ∷ Maybe SymbolSize
  , color ∷ Maybe Color
  , borderColor ∷ Maybe Color
  , label ∷ Maybe AxisLabel
  }

newtype CheckpointStyle
  = CheckpointStyle CheckpointStyleRec

instance checkpointStyleEncodeJson ∷ EncodeJson CheckpointStyle where
  encodeJson (CheckpointStyle obj) =
    encodeJson
      $ SM.fromFoldable
        [ "symbol" := obj.symbol
        , "symbolSize" := obj.symbolSize
        , "color" := obj.color
        , "borderColor" := obj.borderColor
        , "label" := obj.label
        ]
instance checkpointStyleDecodeJson ∷ DecodeJson CheckpointStyle where
  decodeJson j = do
    o ← decodeJson j
    r ← { symbol: _
        , symbolSize: _
        , color: _
        , borderColor: _
        , label: _ }
        <$> (o .? "symbol")
        <*> (o .? "symbolSize")
        <*> (o .? "color")
        <*> (o .? "borderColor")
        <*> (o .? "label")
    pure $ CheckpointStyle r

checkpointStyleDefault ∷ CheckpointStyleRec
checkpointStyleDefault =
  { symbol: Nothing
  , symbolSize: Nothing
  , color: Nothing
  , borderColor: Nothing
  , label: Nothing
  }
