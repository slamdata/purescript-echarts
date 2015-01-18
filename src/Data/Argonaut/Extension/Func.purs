module Data.Argonaut.Extension.Func where

import Data.Argonaut.Encode
import Data.Argonaut.Core
import Data.Function

foreign import encodeFuncs """
function encodeFuncs(fn) {
  return fn;
}
""" :: forall a. a -> Json


instance fn0EncodeJson :: EncodeJson (Fn0 a) where
  encodeJson = encodeFuncs

instance fn1EncodeJson :: EncodeJson (Fn1 a b) where
  encodeJson = encodeFuncs

instance fn2EncodeJson :: EncodeJson (Fn2 a b c) where
  encodeJson = encodeFuncs

instance fn3EncodeJson :: EncodeJson (Fn3 a b c d) where
  encodeJson = encodeFuncs
