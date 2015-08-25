module ECharts.Image (
  ImgType(..),
  getDataURL,
  getImage
  ) where
import Prelude
import DOM
import DOM.Node.Types
import Control.Monad.Eff
import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Either

import ECharts.Chart
import ECharts.Effects


data ImgType = PNG | JPEG
imgStr :: ImgType -> String
imgStr img = case img of
  PNG -> "png"
  JPEG -> "jpeg"

instance encodeImg :: EncodeJson ImgType where
  encodeJson = encodeJson <<< imgStr

instance decodeImg :: DecodeJson ImgType where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "png" -> pure PNG
      "jpeg" -> pure JPEG
      _ -> Left "incorrect img type"

foreign import getDataURLImpl :: forall e. Fn2 String EChart (Eff e String)

getDataURL :: forall e. ImgType -> EChart -> Eff (image::IMAGE_MAKING|e) String
getDataURL img chart = runFn2 getDataURLImpl (imgStr img) chart


foreign import getImageImpl :: forall e. Fn2 String EChart (Eff e Node)

getImage :: forall e. ImgType -> EChart -> Eff (dom::DOM, image::IMAGE_MAKING|e) Node
getImage img chart = runFn2 getImageImpl (imgStr img) chart
