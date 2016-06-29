module ECharts.Color (
  Color(..),
  ColorFuncParamRec(),
  ColorFuncParam(..),
  CalculableColor(..),
  LinearGradient(..),
  LinearGradientInput(),
  linearGradientInputDefault
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

type LinearGradientInput = 
  { x0 :: Number, y0 :: Number, x1 :: Number,  y1 :: Number, 
    s0 :: Number, sc0 :: String, s1 :: Number, sc1 :: String  }

linearGradientInputDefault :: LinearGradientInput
linearGradientInputDefault = {
  x0: 0.0, y0: 0.0, x1: 0.0, y1: 0.0,
  s0: 0.0, sc0: "rgba(255,255,255,0)", s1: 1.0, sc1: "rgba(255,255,255,0)"}


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


