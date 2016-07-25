module ECharts.Loading
  ( LoadingEffect(..)
  , LoadingOption(..)
  , LoadingOptionRec
  , loadingOptionDefault
  , showLoading
  , hideLoading
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.StrMap as SM

import ECharts.Coords (XPos, YPos)
import ECharts.Chart (EChart)
import ECharts.Style.Text (TextStyle)
import ECharts.Effects (ECHARTS)
import ECharts.Utils (unnull)


data LoadingEffect
  = Spin
  | Bar
  | Ring
  | Whirling
  | DynamicLine
  | Bubble

instance loadingEffectEncodeJson ∷ EncodeJson LoadingEffect where
  encodeJson a = encodeJson $ case a of
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
    encodeJson
      $ SM.fromFoldable
        [ "text" := options.text
        , "x" := options.x
        , "y" := options.y
        , "textStyle" := options.textStyle
        , "effect" := options.effect
        , "effectOption" := options.effectOption
        , "progress" := options.progress
        ]


foreign import showLoadingImpl
  ∷ ∀ e. Fn2 Json EChart (Eff (echarts ∷ ECHARTS|e) EChart)

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
