module ECharts.Formatter(
  FormatParams(),
  Formatter(..)
  ) where

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode

import Data.Function

import Control.Monad.Eff


type FormatParams = Json

data Formatter =
  Template String
  | FormatFunc (Array FormatParams -> String)
  | StringFormatFunc (String -> String)
  | NumberFormatFunc (Number -> String)

foreign import func2json :: forall a. a -> Json

foreign import effArrToFn :: forall eff a b. (a -> Eff eff b) -> Fn1 a b

instance formatterEncodeJson :: EncodeJson Formatter where
  encodeJson (Template str) = encodeJson str
  encodeJson (FormatFunc func) = func2json $ func
  encodeJson (StringFormatFunc func) = func2json func
  encodeJson (NumberFormatFunc func) = func2json func

instance formatterDecodeJson :: DecodeJson Formatter where
  decodeJson json = Template <$> decodeJson json

