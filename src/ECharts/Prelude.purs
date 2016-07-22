module ECharts.Prelude
  ( module Prelude

  , module Control.Alt
  , module Control.Monad.Eff

  , module Data.Maybe
  , module Data.Either
  , module Data.Argonaut
  , module Data.Tuple
  ) where


import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, JAssoc, JObject, encodeJson, decodeJson, (.?), (:=))
import Data.Tuple (Tuple(..))
