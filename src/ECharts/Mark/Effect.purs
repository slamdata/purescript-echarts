module ECharts.Mark.Effect where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.StrMap (fromList, StrMap(..))
import ECharts.Common
import ECharts.Color

type MarkPointEffectRec = {
    show :: Maybe Boolean,
    loop :: Maybe Boolean,
    period :: Maybe Boolean,
    scaleSize :: Maybe Boolean,
    color :: Maybe Color,
    shadowBlur :: Maybe Number
  }

newtype MarkPointEffect = MarkPointEffect MarkPointEffectRec
 
instance mpEffectEncodeJson :: EncodeJson MarkPointEffect where
  encodeJson (MarkPointEffect cfg) =
    fromObject $ fromList $
    [
      "show" := cfg.show,
      "loop" := cfg.loop,
      "period" := cfg.period,
      "scaleSize" := cfg.scaleSize,
      "color" := cfg.color,
      "shadowBlur" := cfg.shadowBlur
    ]

instance mpEffectDecodeJson :: DecodeJson MarkPointEffect where
  decodeJson j = do
    o <- decodeJson j
    r <- { show: _
         , loop: _
         , period: _
         , scaleSize: _
         , color: _
         , shadowBlur: _ } <$>
         (o .? "show") <*>
         (o .? "loop") <*>
         (o .? "period") <*>
         (o .? "scaleSize") <*>
         (o .? "color") <*>
         (o .? "shoadowBlur")
    pure $ MarkPointEffect r
markPointEffectDefault :: MarkPointEffectRec
markPointEffectDefault =
  {
    show: Nothing,
    loop: Nothing,
    period: Nothing,
    scaleSize: Nothing,
    color: Nothing,
    shadowBlur: Nothing
  }
