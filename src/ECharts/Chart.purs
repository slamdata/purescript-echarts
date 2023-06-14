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

import Prelude (Unit, ($), (<<<))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (class MonadEff, liftEff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either)
import Data.Foreign (Foreign, toForeign)
-- import DOM (DOM)
import Web.HTML (HTMLElement)
import ECharts.Internal (undefinedValue)
import ECharts.Theme (Theme, builtInThemeName)
import ECharts.Types (Chart, Option)

foreign import initImpl
  ∷ Foreign
  → HTMLElement
  → Effect Chart

init
  ∷ ∀ m
  . MonadEffect m
  ⇒ HTMLElement
  → m Chart
init el = liftEffect $ initImpl undefinedValue el

initWithTheme
  ∷ ∀ m
  . MonadEffect m
  ⇒ Theme
  → HTMLElement
  → m Chart
initWithTheme theme el =
  liftEffect $ initImpl (either (toForeign <<< builtInThemeName) toForeign theme) el

foreign import registerTheme
  ∷ String
  → Foreign
  → Effect Unit

foreign import setOptionImpl
  ∷ Option → Chart → Effect Unit

setOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Option
  → Chart
  → m Unit
setOption opts chart = liftEffect$ setOptionImpl opts chart


foreign import resetOptionImpl
  ∷ Option → Chart → Effect Unit


resetOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Option
  → Chart
  → m Unit
resetOption opts chart = liftEffect$ resetOptionImpl opts chart


foreign import resizeImpl
  ∷ Chart → Effect Unit

resize
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
resize chart = liftEffect$ resizeImpl chart


foreign import clearImpl
  ∷ Chart → Effect Unit

clear
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
clear chart = liftEffect $ clearImpl chart

foreign import disposeImpl
  ∷ Chart → Effect Unit

dispose
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
dispose chart = liftEffect $ disposeImpl chart

foreign import getOptionImpl
  ∷ Chart → Effect Foreign

getOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Foreign
getOption chart = liftEffect $ getOptionImpl chart
