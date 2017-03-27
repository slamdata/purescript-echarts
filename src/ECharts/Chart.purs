module ECharts.Chart
  ( init
  , registerTheme
  , setOption
  , resetOption
  , resize
  , dispose
  , clear
  , getOption
  , Theme(..)
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement)

import ECharts.Internal (undefinedValue)
import ECharts.Monad (DSL, buildObj)
import ECharts.Types.Phantom (OptionI)
import ECharts.Types (Chart, ECHARTS)

foreign import initImpl
  ∷ ∀ e. HTMLElement
  → Foreign
  → Eff (dom ∷ DOM, echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Chart

data Theme = ByName String | FromObject Foreign

init
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM, echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
  ⇒ HTMLElement
  → Maybe Theme
  → m Chart
init el Nothing =  liftEff  $ initImpl el (toForeign undefinedValue)
init el (Just (ByName name)) =  liftEff  $ initImpl el (toForeign name)
init el (Just (FromObject theme)) =  liftEff  $ initImpl el (toForeign theme)

foreign import registerTheme
  ∷ ∀ e. String
  → Foreign
  → Eff (echarts ∷ ECHARTS|e) Unit

foreign import setOptionImpl
  ∷ ∀ e. Foreign → Chart → Eff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Unit

setOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
  ⇒ DSL OptionI
  → Chart
  → m Unit
setOption opts chart = liftEff $ setOptionImpl (buildObj opts) chart


foreign import resetOptionImpl
  ∷ ∀ e. Foreign → Chart → Eff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Unit


resetOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
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
