module ECharts.Loading
  ( LoadingEffect(..)
  , LoadingOption(..)
  , LoadingOptionRec
  , loadingOptionDefault
  , showLoading
  , hideLoading
  ) where

import Prelude

import ECharts.Coords
import ECharts.Chart
import ECharts.Style.Text
import ECharts.Effects
import ECharts.Utils

import Data.Function
import Data.Maybe
import Data.StrMap as M
import Control.Monad.Eff
import Data.List (toList)

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode

data LoadingEffect
  = Spin
  | Bar
  | Ring
  | Whirling
  | DynamicLine
  | Bubble

instance loadingEffectEncodeJson ∷ EncodeJson LoadingEffect where
  encodeJson a = fromString $ case a of
    Spin → "spin"
    Bar → "bar"
    Ring → "ring"
    Whirling → "whirling"
    DynamicLine → "dynamicLine"
    Bubble → "bubble"

type LoadingOptionRec =
  { text ∷ Maybe String
  , x ∷ Maybe XPos
  , y ∷ Maybe YPos
  , textStyle ∷ Maybe TextStyle
  , effect ∷ Maybe LoadingEffect
  , effectOption ∷ Maybe Json
  , progress ∷ Maybe Number
  }

newtype LoadingOption
  = LoadingOption LoadingOptionRec

instance showLoadingOptions ∷ EncodeJson LoadingOption where
  encodeJson (LoadingOption options) =
    fromObject
      $ M.fromList
      $ toList
        [ "text" := options.text
        , "x" := options.x
        , "y" := options.y
        , "textStyle" := options.textStyle
        , "effect" := options.effect
        , "effectOption" := options.effectOption
        , "progress" := options.progress
        ]


foreign import showLoadingImpl
  ∷ ∀ e. Fn2 Json EChart (Eff (showLoadingECharts∷LOADING_SHOW|e) EChart)

showLoading
  ∷ ∀ e
  . LoadingOption
  → EChart
  → Eff (echarts ∷ ECHARTS|e) EChart
showLoading opts chart =
  runFn2 showLoadingImpl (unnull (encodeJson opts)) chart


foreign import hideLoading
  ∷ ∀ e. EChart → Eff (echarts ∷ ECHARTS|e) EChart

loadingOptionDefault ∷ LoadingOptionRec
loadingOptionDefault =
  { text: Nothing
  , x: Nothing
  , y: Nothing
  , textStyle: Nothing
  , effect: Nothing
  , effectOption: Nothing
  , progress: Nothing
  }
