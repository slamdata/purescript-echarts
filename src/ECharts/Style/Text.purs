module ECharts.Style.Text where

import Data.Array (concat)
import Data.Maybe
import Data.Argonaut.Core
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

data FontStyle = FSNormal | FSItalic | FSOblique
instance fontStyleEncodeJson :: EncodeJson FontStyle where
  encodeJson a = fromString $ case a of 
    FSNormal -> "normal"
    FSItalic -> "italic"
    FSOblique -> "oblique"

    
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
    
newtype TextStyle = 
  TextStyle {
    color :: Maybe Color,
    decoration :: Maybe Decoration,
    align :: Maybe HorizontalAlign,
    baseline :: Maybe TextBaseline,
    fontFamily :: Maybe FontFamily,
    fontSize :: Maybe Number,
    fontStyle :: Maybe FontStyle,
    fontWeight :: Maybe FontWeight
  }



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
