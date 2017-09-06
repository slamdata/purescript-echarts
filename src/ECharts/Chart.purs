module ECharts.Chart
  ( init
  , initWithTheme
  , registerTheme
  , setOption
  , resetOption
  , resize
  , dispose
  , clear
  , getOption
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either)
import Data.Foreign (Foreign, toForeign)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import ECharts.Internal (undefinedValue)
import ECharts.Monad (DSL, buildObj)
import ECharts.Theme (Theme, builtInThemeName)
import ECharts.Types (Chart, ECHARTS)
import ECharts.Types.Phantom (OptionI)

foreign import initImpl
  ∷ ∀ e. Foreign
  → HTMLElement
  → Eff (dom ∷ DOM, echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) Chart

init
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM, echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) m
  ⇒ HTMLElement
  → m Chart
init el = liftEff $ initImpl undefinedValue el

initWithTheme
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM, echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) m
  ⇒ Theme
  → HTMLElement
  → m Chart
initWithTheme theme el =
  liftEff $ initImpl (either (toForeign <<< builtInThemeName) toForeign theme) el

foreign import registerTheme
  ∷ ∀ e. String
  → Foreign
  → Eff (echarts ∷ ECHARTS|e) Unit

foreign import setOptionImpl
  ∷ ∀ e. Foreign → Chart → Eff (echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) Unit

setOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) m
  ⇒ DSL OptionI
  → Chart
  → m Unit
setOption opts chart = liftEff $ setOptionImpl (buildObj opts) chart


foreign import resetOptionImpl
  ∷ ∀ e. Foreign → Chart → Eff (echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) Unit


resetOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, exception ∷ EXCEPTION|e) m
  ⇒ DSL OptionI
  → Chart
  → m Unit
resetOption opts chart = liftEff $ resetOptionImpl (buildObj opts) chart


foreign import resizeImpl
  ∷ ∀ e. Chart → Eff (echarts ∷ ECHARTS|e) Unit

resize
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS|e) m
  ⇒ Chart
  → m Unit
resize chart = liftEff $ resizeImpl chart


foreign import clearImpl
  ∷ ∀ e. Chart → Eff (echarts ∷ ECHARTS|e) Unit

clear
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS|e) m
  ⇒ Chart
  → m Unit
clear chart = liftEff $ clearImpl chart

foreign import disposeImpl
  ∷ ∀ e. Chart → Eff (echarts ∷ ECHARTS|e) Unit

dispose
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS|e) m
  ⇒ Chart
  → m Unit
dispose chart = liftEff $ disposeImpl chart

foreign import getOptionImpl
  ∷ ∀ e. Chart → Eff (echarts ∷ ECHARTS|e) Foreign

getOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS|e) m
  ⇒ Chart
  → m Foreign
getOption chart = liftEff $ getOptionImpl chart
