module ECharts.Common where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Maybe
import Data.Either
import Data.Array ((!!))
import Data.List (toList)

import qualified Data.String as S
import Data.Function
import Data.Tuple
import qualified Data.StrMap as M
import Global

type GeoCoord = M.StrMap (Tuple Number Number)


data Corner a = AllCorners a | Corners a a a a
instance cornerJsonEncode :: (EncodeJson a) => EncodeJson (Corner a) where
  encodeJson corners = case corners of
    AllCorners a -> encodeJson a
    Corners a b c d -> encodeJson [a, b, c, d]

instance cornerJsonDecode :: (DecodeJson a) => DecodeJson (Corner a) where
  decodeJson json =
    (do arr <- decodeJson json
        maybe (Left "incorrect corners") Right $ do
          a <- arr !! 0
          b <- arr !! 1
          c <- arr !! 2
          d <- arr !! 3
          pure $ Corners a b c d)
    <|>
    (AllCorners <$> decodeJson json)


data PercentOrPixel = Percent Number | Pixel Number
instance percentOrPixelEncodeJson :: EncodeJson PercentOrPixel where
  encodeJson (Percent percent) = encodeJson $ show percent <> "%"
  encodeJson (Pixel pixels) = encodeJson pixels

instance percentOrPixelDecodeJson :: DecodeJson PercentOrPixel where
  decodeJson json =
    (do str <- decodeJson json
        if ((S.lastIndexOf "%" str) == (Just $ (S.length str) - 1)) &&
           (not $ isNaN $ readInt 10 str)
          then pure $ Percent (readInt 10 str)
          else Left "incorrect percent")
    <|>
    (Pixel <$> decodeJson json)


data RoseType = RTRadius | RTArea
instance roseTypeEncodeJson :: EncodeJson RoseType where
  encodeJson a = encodeJson $ case a of
    RTRadius -> "radius"
    RTArea -> "area"

instance roseTypeDecodeJson :: DecodeJson RoseType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "radius" -> pure RTRadius
      "area" -> pure RTArea
      _ -> Left "incorrect rose type"


data SelectedMode = SelModeSingle | SelModeMultiple | SelModeFalse
instance selModeEncodeJson :: EncodeJson SelectedMode where
  encodeJson a = case a of
    SelModeSingle -> encodeJson "single"
    SelModeMultiple -> encodeJson "multiple"
    SelModeFalse -> encodeJson false

instance selModeDecodeJson :: DecodeJson SelectedMode where
  decodeJson json =
    (do str <- decodeJson json
        case str of
          "single" -> pure SelModeSingle
          "multiple" -> pure SelModeMultiple
          _ -> Left "incorrect select mode")
    <|>
    (do fls <- decodeJson json
        if not fls
          then pure SelModeFalse
          else Left "incorrect select mode")
          


data MapValueCalculation = SumCalculation | AverageCalculation
instance mapValueCalculationEncodeJson :: EncodeJson MapValueCalculation where
  encodeJson a = encodeJson $ case a of
    SumCalculation -> "sum"
    AverageCalculation -> "average"

instance mapValueCalculationDecodeJson :: DecodeJson MapValueCalculation where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "sum" -> pure SumCalculation
      "average" -> pure AverageCalculation
      _ -> Left "incorrect map value calculation"


data Roam = Enable | Disable | Scale | Move
instance roamEncodeJson :: EncodeJson Roam where
  encodeJson Enable = encodeJson true
  encodeJson Disable = encodeJson false
  encodeJson Scale = encodeJson "scale"
  encodeJson Move = encodeJson "move"


instance roamDecodeJson :: DecodeJson Roam where
  decodeJson j =
    (do str <- decodeJson j
        case str of
          "scale" -> pure Scale
          "move" -> pure Move
          _ -> Left "incorrect roam")
    <|>
    (do bl <- decodeJson j
        if bl
          then pure Enable
          else pure Disable)
          

type MinMaxRec = {
  min :: Number,
  max :: Number
  }

newtype MinMax = MinMax MinMaxRec

instance minMaxEncodeJson :: EncodeJson MinMax where
  encodeJson (MinMax mm) = fromObject $ M.fromList $ toList $ [
    "min" := mm.min,
    "max" := mm.max
    ]

instance minMaxDecodeJson :: DecodeJson MinMax where
  decodeJson j = do
    o <- decodeJson j
    r <- {min: _, max: _} <$> (o .? "min") <*> (o .? "max")
    pure $ MinMax r

type Center = Tuple PercentOrPixel PercentOrPixel


type RsRec = {inner :: PercentOrPixel, outer :: PercentOrPixel}
data Radius = R PercentOrPixel | Rs RsRec
instance radiusEncodeJson :: EncodeJson Radius where
  encodeJson (R num) = encodeJson num
  encodeJson (Rs {inner = i, outer = o}) = encodeJson [i, o]

instance radiusDecodeJson :: DecodeJson Radius where
  decodeJson j =
    (R <$> decodeJson j) <|>
    (do arr <- decodeJson j
        maybe (Left "incorrect radius") Right do
          i <- arr !! 0
          o <- arr !! 1
          pure $ Rs {inner: i, outer: o})

data Sort = NoSort | Asc | Desc
instance sortEncodeJson :: EncodeJson Sort where
  encodeJson a = encodeJson $ case a of
    NoSort -> "none"
    Asc -> "ascending"
    Desc -> "descending"

instance sortDecodeJson :: DecodeJson Sort where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "none" -> pure NoSort
      "ascending" -> pure Asc
      "descending" -> pure Desc
      _ -> Left "incorrect sort"


data Interval = Auto | Custom Number
instance intervalEncodeJson :: EncodeJson Interval where
  encodeJson Auto = encodeJson "auto"
  encodeJson (Custom num) = encodeJson num

instance intervalDecodeJson :: DecodeJson Interval where
  decodeJson j =
    (do str <- decodeJson j
        if str == "auto"
          then pure Auto
          else Left "incorrect interval") <|>
    (Custom <$> decodeJson j)
