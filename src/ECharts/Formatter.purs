module ECharts.Formatter(
  FormatParams(),
  Formatter(..),
  GenericFormatter(..)
  ) where

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode


import Data.Function

import Control.Monad.Eff

type FormatParams = Json

foreign import data GenericFormatter :: *

data Formatter =
  Template String
  | FormatFunc (forall eff. Array FormatParams -> Eff eff String)
  | StringFormatFunc (String -> String)
  | NumberFormatFunc (Number -> String)
  | GenericFormatFunc GenericFormatter

foreign import func2json :: forall a. a -> Json

foreign import effArrToFn :: forall eff a b. (a -> Eff eff b) -> Fn1 a b

instance formatterEncodeJson :: EncodeJson Formatter where
  encodeJson (Template str) = encodeJson str
  encodeJson (FormatFunc func) = func2json $ effArrToFn func
  encodeJson (StringFormatFunc f) = func2json f
  encodeJson (NumberFormatFunc f) = func2json f
  encodeJson (GenericFormatFunc f) = func2json f

instance formatterDecodeJson :: DecodeJson Formatter where
  decodeJson json = Template <$> decodeJson json

