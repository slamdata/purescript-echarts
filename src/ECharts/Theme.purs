module ECharts.Theme
  ( Theme(..)
  , BuiltInTheme
  , builtInThemeName
  , dark
  , infographic
  , macarons
  , roma
  , shine
  , vintage
  ) where

import Data.Either (Either(..))
import Data.Foreign (Foreign)

type Theme = Either BuiltInTheme Foreign

foreign import data BuiltInTheme :: Type

foreign import builtInThemeName :: BuiltInTheme -> String

dark :: Theme
dark = Left _dark

infographic :: Theme
infographic = Left _infographic

macarons :: Theme
macarons = Left _macarons

roma :: Theme
roma = Left _roma

shine :: Theme
shine = Left _shine

vintage :: Theme
vintage = Left _vintage

foreign import _dark :: BuiltInTheme
foreign import _infographic :: BuiltInTheme
foreign import _macarons :: BuiltInTheme
foreign import _roma :: BuiltInTheme
foreign import _shine :: BuiltInTheme
foreign import _vintage :: BuiltInTheme
