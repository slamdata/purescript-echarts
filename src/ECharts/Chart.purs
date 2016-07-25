module ECharts.Chart
  ( EChart
  , ZRender
  , Theme(..)
  , init
  , setTheme
  , getZRender
  , resize
  , refresh
  , clear
  , dispose
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn2, runFn2)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement())


import ECharts.Effects (ECHARTS)

foreign import data EChart ∷ *
foreign import data ZRender ∷ *

data Theme
  = ThemeName String
  | ThemeConfig Json

instance themeEncodeJson ∷ EncodeJson Theme where
  encodeJson = case _ of
    ThemeName name → encodeJson name
    ThemeConfig a → encodeJson a

foreign import initImpl
  ∷ ∀ e. Fn2 HTMLElement Json (Eff (dom ∷ DOM, echarts ∷ ECHARTS|e) EChart)

init
 ∷ ∀ e
 . Maybe Theme
 → HTMLElement
 → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) EChart
init theme dom =
  runFn2 initImpl dom (encodeJson theme)

foreign import setThemeImpl
  ∷ ∀ e. Fn2 Json EChart (Eff e EChart)

setTheme
  ∷ ∀ e
  . Theme
  → EChart
  → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) EChart
setTheme theme chart = do
  runFn2 setThemeImpl (encodeJson theme) chart


foreign import getZRender
  ∷ ∀ e. EChart → Eff e ZRender

foreign import resize
  ∷ ∀ e. EChart → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) Unit

foreign import refresh
 ∷ ∀ e
 . EChart
 → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) Unit

foreign import clear
  ∷ ∀ e
  . EChart
  → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) Unit

foreign import dispose
  ∷ ∀ e
  . EChart
  → Eff (dom ∷ DOM, echarts ∷ ECHARTS |e) Unit
