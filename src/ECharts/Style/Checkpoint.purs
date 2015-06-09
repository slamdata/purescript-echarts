module ECharts.Style.Checkpoint where

import Data.Maybe
import Data.StrMap
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import ECharts.Symbol
import ECharts.Color
import ECharts.Axis

type CheckpointStyleRec = {
    symbol :: Maybe Symbol,
    symbolSize :: Maybe SymbolSize,
    color :: Maybe Color,
    borderColor :: Maybe Color,
    label :: Maybe AxisLabel
    }

newtype CheckpointStyle = CheckpointStyle CheckpointStyleRec
   

instance checkpointStyleEncodeJson :: EncodeJson CheckpointStyle where
  encodeJson (CheckpointStyle obj) =
    fromObject $ fromList $
    [
      "symbol" := obj.symbol,
      "symbolSize" := obj.symbolSize,
      "color" := obj.color,
      "borderColor" := obj.borderColor,
      "label" := obj.label
    ]
instance checkpointStyleDecodeJson :: DecodeJson CheckpointStyle where
  decodeJson j = do
    o <- decodeJson j
    r <- { symbol: _
         , symbolSize: _
         , color: _
         , borderColor: _
         , label: _ } <$>
         (o .? "symbol") <*>
         (o .? "symbolSize") <*>
         (o .? "color") <*>
         (o .? "borderColor") <*>
         (o .? "label")
    pure $ CheckpointStyle r
    
checkpointStyleDefault :: CheckpointStyleRec
checkpointStyleDefault = {
  symbol: Nothing,
  symbolSize: Nothing,
  color: Nothing,
  borderColor: Nothing,
  label: Nothing
  }
