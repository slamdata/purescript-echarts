module ECharts.Style.Link where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (Color)

data LinkType
  = LTCurve
  | LTLine

instance linkTypeEncodeJson ∷ EncodeJson LinkType where
  encodeJson a = encodeJson $ case a of
    LTCurve → "curve"
    LTLine → "line"


instance linkTypeDecodeJson ∷ DecodeJson LinkType where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "curve" → pure LTCurve
      "line" → pure LTLine
      _ → Left "incorrect link type"

type LinkStyleRec =
  { "type" ∷ Maybe LinkType
  , color ∷ Maybe Color
  , width ∷ Maybe Number
  }


newtype LinkStyle
  = LinkStyle LinkStyleRec

instance linkStyleEncodeJson ∷ EncodeJson LinkStyle where
  encodeJson (LinkStyle ls) =
    encodeJson
      $ SM.fromFoldable
        [ "type" := ls."type"
        , "color" := ls.color
        , "width" := ls.width
        ]

instance linkStyleDecodeJson ∷ DecodeJson LinkStyle where
  decodeJson j = do
    o ← decodeJson j
    r ← { "type": _
        , color: _
        , width: _ }
        <$> (o .? "type")
        <*> (o .? "color")
        <*> (o .? "width")
    pure $ LinkStyle r

linkStyleDefault ∷ LinkStyleRec
linkStyleDefault =
  { "type": Nothing
  , color: Nothing
  , width: Nothing
  }
