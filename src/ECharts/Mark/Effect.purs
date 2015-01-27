module ECharts.Mark.Effect where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.StrMap (fromList, StrMap(..))
import ECharts.Common
import ECharts.Color

newtype MarkPointEffect =
  MarkPointEffect {
    show :: Maybe Boolean,
    loop :: Maybe Boolean,
    period :: Maybe Boolean,
    scaleSize :: Maybe Boolean,
    color :: Maybe Color,
    shadowBlur :: Maybe Number
  }
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
markPointEffectDefault =
  {
    show: Nothing,
    loop: Nothing,
    period: Nothing,
    scaleSize: Nothing,
    color: Nothing,
    shadowBlur: Nothing
  }
