module ECharts.Color (
  Color(..),
  ColorFuncParamRec(),
  ColorFuncParam(..),
  CalculableColor(..)
  ) where

import Prelude
import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import ECharts.Item.Value
import ECharts.Utils

type Color = String

foreign import func2json :: forall a. a -> Json

type ColorFuncParamRec = {
    seriesIndex :: Number,
    series :: String,
    dataIndex :: Number,
    "data" :: {
      value :: ItemValue,
      name :: String
      }
  }

newtype ColorFuncParam = ColorFuncParam ColorFuncParamRec



data CalculableColor = SimpleColor Color | ColorFunc (ColorFuncParam -> Color)
instance calculableColorEncodeJson :: EncodeJson CalculableColor where
  encodeJson cc = case cc of
    SimpleColor color -> encodeJson color
    ColorFunc func -> func2json $ mkFn1 func

instance calculableColorDecodeJson :: DecodeJson CalculableColor where
  decodeJson j = SimpleColor <$> decodeJson j
