module ECharts.Internal where

import Prelude

import Data.Foreign (Foreign)

foreign import unsafeSetField
  ∷ Foreign → String → Foreign → Foreign

foreign import emptyObject
  ∷ Unit → Foreign
