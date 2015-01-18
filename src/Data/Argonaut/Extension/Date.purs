module Data.Argonaut.Extension.Date where

import Data.Date (JSDate(), Date(), toJSDate)
import Data.Argonaut.Core
import Data.Argonaut.Encode


foreign import jsDateToJson """
function jsDateToJson(a) {return a;}
""" :: JSDate -> Json


instance dateEncodeJson :: EncodeJson Date where
  encodeJson date = jsDateToJson $ toJSDate date

       
