module Data.Foreign where

import Foreign as F

type Foreign = F.Foreign

toForeign :: forall a. a -> Foreign
toForeign = F.unsafeToForeign
