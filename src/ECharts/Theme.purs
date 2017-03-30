module ECharts.Theme
  ( Theme(..)
  , dark
  , infographic
  , macarons
  , roma
  , shine
  , vintage
  , BuiltInTheme(..)
  , builtInToTheme
  ) where

import Prelude

import Data.Foreign (Foreign, ForeignError(..), fail, readString)
import Data.Foreign.Class (class IsForeign)

foreign import forceExport ∷ Foreign

data Theme = ByName String | FromObject Foreign
data BuiltInTheme = Infographic | Macarons | Roma | Shine | Vintage | Dark

instance builtInThemeIsForeign ∷ IsForeign BuiltInTheme where
  read f = do
    str ← readString f
    case str of
      "infographic" → pure Infographic
      "macarons" → pure Macarons
      "roma" → pure Roma
      "shine" → pure Shine
      "vintage" → pure Vintage
      "dark" → pure Dark
      _ → fail $ ForeignError $ "Theme `" <> str <> "` is not supported"

builtInToTheme ∷ BuiltInTheme → Theme
builtInToTheme = case _ of
  Infographic → ByName "infographic"
  Macarons → ByName "macarons"
  Roma → ByName "roma"
  Shine → ByName "shine"
  Vintage → ByName "vintage"
  Dark → ByName "dark"

dark ∷ Theme
dark = builtInToTheme Dark

infographic ∷ Theme
infographic = builtInToTheme Infographic

macarons ∷ Theme
macarons = builtInToTheme Macarons

roma ∷ Theme
roma = builtInToTheme Roma

shine ∷ Theme
shine = builtInToTheme Shine

vintage ∷ Theme
vintage = builtInToTheme Vintage
