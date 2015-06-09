module ECharts.Style.Text where

import Data.Array (concat)
import Data.Maybe
import Data.Either
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import qualified Data.StrMap as M


import ECharts.Common
import ECharts.Color
import ECharts.Coords

type Decoration = String
type FontFamily = String

data TextBaseline = TBLTop
                  | TBLBottom
                  | TBLMiddle

instance textBaselineEncodeJson :: EncodeJson TextBaseline where
  encodeJson a = fromString $ case a of 
    TBLTop -> "top"
    TBLBottom -> "bottom"
    TBLMiddle -> "middle"

instance textBaselineDecodeJson :: DecodeJson TextBaseline where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "top" -> pure TBLTop
      "bottom" -> pure TBLBottom
      "middle" -> pure TBLMiddle
      _ -> Left "incorrect text base line" 

data FontStyle = FSNormal | FSItalic | FSOblique
instance fontStyleEncodeJson :: EncodeJson FontStyle where
  encodeJson a = fromString $ case a of 
    FSNormal -> "normal"
    FSItalic -> "italic"
    FSOblique -> "oblique"

instance fontStyleDecodeJson :: DecodeJson FontStyle where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "normal" -> pure FSNormal
      "italic" -> pure FSItalic
      "oblique" -> pure FSOblique
      _ -> Left "incorrect font style"

data FontWeight = FWNormal
                | FWBold
                | FWBolder
                | FWLighter
                | FW100
                | FW200
                | FW300
                | FW400
                | FW500
                | FW600
                | FW700
                | FW800
                | FW900

instance fontWeightEncodeJson :: EncodeJson FontWeight where
  encodeJson a = fromString $ case a of 
    FWNormal -> "normal"
    FWBold -> "bold"
    FWBolder -> "bolder"
    FWLighter -> "lighter"
    FW100 -> "100"
    FW200 -> "200"
    FW300 -> "300"
    FW400 -> "400"
    FW500 -> "500"
    FW600 -> "600"
    FW700 -> "700"
    FW800 -> "800"
    FW900 -> "900"


instance fontWeightDecodeJson :: DecodeJson FontWeight where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "normal" -> pure FWNormal
      "bold" -> pure FWBold
      "bolder" -> pure FWBolder
      "lighter" -> pure FWLighter
      "100" -> pure FW100
      "200" -> pure FW200
      "300" -> pure FW300
      "400" -> pure FW400
      "500" -> pure FW500
      "600" -> pure FW600
      "700" -> pure FW700
      "800" -> pure FW800
      "900" -> pure FW900
      _ -> Left "incorrect font weight"

type TextStyleRec = {
    color :: Maybe Color,
    decoration :: Maybe Decoration,
    align :: Maybe HorizontalAlign,
    baseline :: Maybe TextBaseline,
    fontFamily :: Maybe FontFamily,
    fontSize :: Maybe Number,
    fontStyle :: Maybe FontStyle,
    fontWeight :: Maybe FontWeight
  }

newtype TextStyle = TextStyle TextStyleRec

instance textStyleEncodeJson :: EncodeJson TextStyle where
  encodeJson (TextStyle ts) =
    fromObject $ M.fromList [
      "color" := ts.color,
      "decoration" := ts.decoration,
      "align" := ts.align,
      "baseline" := ts.baseline,
      "fontFamily" := ts.fontFamily,
      "fontSize" := ts.fontSize,
      "fontStyle" := ts.fontStyle,
      "fontWeight" := ts.fontWeight
    ]

instance textStyleDecodeJson :: DecodeJson TextStyle where
  decodeJson j = do
    o <- decodeJson j
    r <- { color: _
         , decoration: _
         , align: _
         , baseline: _
         , fontFamily: _
         , fontSize: _
         , fontStyle: _
         , fontWeight: _ } <$>
         (o .? "color") <*>
         (o .? "decoration") <*>
         (o .? "align") <*>
         (o .? "baseline") <*>
         (o .? "fontFamily") <*>
         (o .? "fontSize") <*>
         (o .? "fontStyle") <*>
         (o .? "fontWeight")
    pure $ TextStyle r

textStyleDefault :: TextStyleRec
textStyleDefault =
  {
    color: Nothing,
    decoration: Nothing,
    align: Nothing,
    baseline: Nothing,
    fontFamily: Nothing,
    fontSize: Nothing,
    fontStyle: Nothing,
    fontWeight: Nothing
  }
