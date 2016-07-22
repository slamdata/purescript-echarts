module ECharts.Formatter
  ( FormatParams
  , Formatter(..)
  ) where

import ECharts.Prelude

import Unsafe.Coerce (unsafeCoerce)

type FormatParams = Json

data Formatter
  = Template String
  | FormatFunc (Array FormatParams → String)
  | StringFormatFunc (String → String)
  | NumberFormatFunc (Number → String)

func2json ∷ ∀ a b. (a → b) → Json
func2json = unsafeCoerce

instance formatterEncodeJson ∷ EncodeJson Formatter where
  encodeJson (Template str) = encodeJson str
  encodeJson (FormatFunc func) = func2json func
  encodeJson (StringFormatFunc func) = func2json func
  encodeJson (NumberFormatFunc func) = func2json func

instance formatterDecodeJson ∷ DecodeJson Formatter where
  decodeJson json = Template <$> decodeJson json
