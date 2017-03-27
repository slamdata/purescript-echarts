module ECharts.Theme
  ( Theme(..)
  , dark
  , infographic
  , macarons
  , roma
  , shine
  , vintage
  ) where

import Data.Foreign (Foreign)

foreign import forceExport :: Foreign

data Theme = ByName String | FromObject Foreign

dark ∷ Theme
dark = ByName "dark"

infographic ∷ Theme
infographic = ByName "infographic"

macarons ∷ Theme
macarons = ByName "macarons"

roma ∷ Theme
roma = ByName "roma"

shine ∷ Theme
shine = ByName "shine"

vintage ∷ Theme
vintage = ByName "vintage"
