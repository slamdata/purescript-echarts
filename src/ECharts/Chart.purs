module ECharts.Chart
  ( Chart
  , init
  , setOption
  , resetOption
  , resize
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM (DOM)
import DOM.Node.Types (Element)

import ECharts.Effects (ECHARTS)
import ECharts.Types (Option)

foreign import data Chart ∷ *

foreign import initImpl
  ∷ ∀ e. Element → Eff (dom ∷ DOM, echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Chart

init
  ∷ ∀ m e
  . MonadEff (dom ∷ DOM, echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
  ⇒ Element
  → m Chart
init el = liftEff $ initImpl el

foreign import setOptionImpl
  ∷ ∀ e. Option → Chart → Eff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Unit

setOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
  ⇒ Option
  → Chart
  → m Unit
setOption opts chart = liftEff $ setOptionImpl opts chart


foreign import resetOptionImpl
  ∷ ∀ e. Option → Chart → Eff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) Unit


resetOption
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS, err ∷ EXCEPTION|e) m
  ⇒ Option
  → Chart
  → m Unit
resetOption opts chart = liftEff $ resetOptionImpl opts chart


foreign import resizeImpl
  ∷ ∀ e. Chart → Eff (echarts ∷ ECHARTS|e) Unit

resize
  ∷ ∀ m e
  . MonadEff (echarts ∷ ECHARTS|e) m
  ⇒ Chart
  → m Unit
resize chart = liftEff $ resizeImpl chart
