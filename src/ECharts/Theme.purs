module ECharts.Theme
  ( Theme(..)
  , dark
  , infographic
  , macarons
  , roma
  , shine
  , vintage
  , BuiltInTheme(..)
  , parseBuiltInTheme
  , builtInToTheme
  ) where

import Prelude (($), (<>))
import Data.Either (Either(..))
import Data.Foreign (Foreign)

foreign import forceExport ∷ Foreign

data Theme = ByName String | FromObject Foreign
data BuiltInTheme = Infographic | Macarons | Roma | Shine | Vintage | Dark

parseBuiltInTheme :: String -> Either String BuiltInTheme
parseBuiltInTheme str = case str of
  "infographic" → Right Infographic
  "macarons" → Right Macarons
  "roma" → Right Roma
  "shine" → Right Shine
  "vintage" → Right Vintage
  "dark" → Right Dark
  _ → Left $ "`" <> str <> "`is not builtin theme"

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
