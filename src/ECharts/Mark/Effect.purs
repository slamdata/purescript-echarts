module ECharts.Mark.Effect where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (Color)

type MarkPointEffectRec =
  { show ∷ Maybe Boolean
  , loop ∷ Maybe Boolean
  , period ∷ Maybe Boolean
  , scaleSize ∷ Maybe Boolean
  , color ∷ Maybe Color
  , shadowBlur ∷ Maybe Number
  }

newtype MarkPointEffect
  = MarkPointEffect MarkPointEffectRec

instance mpEffectEncodeJson ∷ EncodeJson MarkPointEffect where
  encodeJson (MarkPointEffect cfg) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := cfg.show
        , "loop" := cfg.loop
        , "period" := cfg.period
        , "scaleSize" := cfg.scaleSize
        , "color" := cfg.color
        , "shadowBlur" := cfg.shadowBlur
        ]

instance mpEffectDecodeJson ∷ DecodeJson MarkPointEffect where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , loop: _
        , period: _
        , scaleSize: _
        , color: _
        , shadowBlur: _ }
        <$> (o .? "show")
        <*> (o .? "loop")
        <*> (o .? "period")
        <*> (o .? "scaleSize")
        <*> (o .? "color")
        <*> (o .? "shoadowBlur")
    pure $ MarkPointEffect r


markPointEffectDefault ∷ MarkPointEffectRec
markPointEffectDefault =
  { show: Nothing
  , loop: Nothing
  , period: Nothing
  , scaleSize: Nothing
  , color: Nothing
  , shadowBlur: Nothing
  }
