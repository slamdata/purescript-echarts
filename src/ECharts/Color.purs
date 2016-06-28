module ECharts.Color (
  Color(..),
  ColorFuncParamRec(),
  ColorFuncParam(..),
  CalculableColor(..),
  LinearGradient(..)
  ) where

import Prelude
import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Maybe
import Data.Either

import Control.Monad.Eff

import ECharts.Item.Value


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

foreign import data LinearGradient :: *


data CalculableColor = 
  SimpleColor Color 
  | ColorFunc (String -> Color) 
  | GradientColor LinearGradient

instance calculableColorEncodeJson :: EncodeJson CalculableColor where
  encodeJson cc = case cc of
    SimpleColor color -> encodeJson color
    ColorFunc func -> func2json $ mkFn1 func
    GradientColor gc -> func2json gc

instance calculableColorDecodeJson :: DecodeJson CalculableColor where
  decodeJson j = SimpleColor <$> decodeJson j


