module ECharts.Common where

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Maybe

import Data.Function
import Data.Tuple
import qualified Data.StrMap as M

type GeoCoord = M.StrMap (Tuple Number Number)


data Corner a = AllCorners a | Corners a a a a
instance cornerJsonEncode :: (EncodeJson a) => EncodeJson (Corner a) where
  encodeJson corners = case corners of
    AllCorners a -> encodeJson a
    Corners a b c d -> encodeJson [a, b, c, d]


data PercentOrPixel = Percent Number | Pixel Number
instance percentOrPixelEncodeJson :: EncodeJson PercentOrPixel where
  encodeJson (Percent percent) = encodeJson $ show percent <> "%"
  encodeJson (Pixel pixels) = encodeJson pixels


data RoseType = RTRadius | RTArea
instance roseTypeEncodeJson :: EncodeJson RoseType where
  encodeJson a = encodeJson $ case a of
    RTRadius -> "radius"
    RTArea -> "area"



data SelectedMode = SelModeSingle | SelModeMultiple | SelModeFalse
instance selModeEncodeJson :: EncodeJson SelectedMode where
  encodeJson a = case a of
    SelModeSingle -> encodeJson "single"
    SelModeMultiple -> encodeJson "multiple"
    SelModeFalse -> encodeJson false


data MapValueCalculation = SumCalculation | AverageCalculation
instance mapValueCalculationEncodeJson :: EncodeJson MapValueCalculation where
  encodeJson a = encodeJson $ case a of
    SumCalculation -> "sum"
    AverageCalculation -> "average"


data Roam = Enable | Disable | Scale | Move
instance roamEncodeJson :: EncodeJson Roam where
  encodeJson Enable = encodeJson true
  encodeJson Disable = encodeJson false
  encodeJson Scale = encodeJson "scale"
  encodeJson Move = encodeJson "move"


type MinMaxRec = {
  min :: Number,
  max :: Number
  }

newtype MinMax = MinMax MinMaxRec

instance minMaxEncodeJson :: EncodeJson MinMax where
  encodeJson (MinMax mm) = fromObject $ M.fromList $ [
    "min" := mm.min,
    "max" := mm.max
    ]

type Center = Tuple PercentOrPixel PercentOrPixel

data Radius = R PercentOrPixel | Rs {inner :: PercentOrPixel, outer :: PercentOrPixel}
instance radiusEncodeJson :: EncodeJson Radius where
  encodeJson (R num) = encodeJson num
  encodeJson (Rs {inner = i, outer = o}) = encodeJson [i, o]


data Sort = NoSort | Asc | Desc
instance sortEncodeJson :: EncodeJson Sort where
  encodeJson a = encodeJson $ case a of
    NoSort -> "none"
    Asc -> "ascending"
    Desc -> "descending"


data Interval = Auto | Custom Number
instance intervalEncodeJson :: EncodeJson Interval where
  encodeJson Auto = encodeJson "auto"
  encodeJson (Custom num) = encodeJson num
