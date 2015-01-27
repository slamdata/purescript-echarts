module ECharts.Formatter(
  FormatParams(),
  Formatter(..)
  ) where

import Data.Maybe
import Data.StrMap (fromList, StrMap (..))
import Data.Tuple
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators


import Data.Function

import Control.Monad.Eff

import ECharts.Utils
import ECharts.Common
import ECharts.Item.Value

type FormatParams = Json

data Formatter =
  Template String
  | FormatFunc (forall eff . [FormatParams] -> Eff eff String)


foreign import effArrToFn """
function effArrToFn(arr) {
  return function(x) {
    arr(x)();
  };
}
""" :: forall eff a b. (a -> Eff eff b) -> Fn1 a b
                
instance formatterEncodeJson :: EncodeJson Formatter where
  encodeJson (Template str) = encodeJson str
  encodeJson (FormatFunc func) = func2json $ effArrToFn func



