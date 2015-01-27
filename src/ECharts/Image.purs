module ECharts.Image (
  ImgType(..),
  getDataURL,
  getImage
  ) where 

import DOM
import Control.Monad.Eff
import Data.Function
import Data.Argonaut.Core
import Data.Argonaut.Encode

import ECharts.Chart 
import ECharts.Effects


data ImgType = PNG | JPEG

instance imgTypeShow :: Show ImgType where
  show PNG = "png"
  show JPEG = "jpeg"

instance encodeImg :: EncodeJson ImgType where
  encodeJson = encodeJson <<< show



foreign import getDataURLImpl """
function getDataURLImpl(imgType, chart) {
  return function() {
    return chart.getDataURL(imgType);
  };
}
""" :: forall e. Fn2 String EChart (Eff e String)

getDataURL :: forall e. ImgType -> EChart -> Eff (image::ImageMaking|e) String
getDataURL img chart = runFn2 getDataURLImpl (show img) chart


foreign import getImageImpl """
function getImageImpl(imgType, chart) {
  return function( ){
    return chart.getImage(imgType);
  };
}
""" :: forall e. Fn2 String EChart (Eff e Node)

getImage :: forall e. ImgType -> EChart -> Eff (dom::DOM, image::ImageMaking|e) Node
getImage img chart = runFn2 getImageImpl (show img) chart
