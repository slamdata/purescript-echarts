module ECharts.Color where

import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Extension.Func

import ECharts.Item.Value

type Color = String




newtype ColorFuncParam =
  ColorFuncParam {
    "seriesIndex" :: Number,
    "series" :: String,
    "dataIndex" :: Number,
    "data" :: {
      value :: ItemValue,
      name :: String
      }
  }

data CalculableColor = SimpleColor Color | ColorFunc (ColorFuncParam -> Color)

instance calculableColorEncodeJson :: EncodeJson CalculableColor where
  encodeJson cc = case cc of
    SimpleColor color -> encodeJson color
    ColorFunc func -> encodeJson $ mkFn1 func
