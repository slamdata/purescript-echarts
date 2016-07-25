module ECharts.Connect
  ( connect
  , Connection
  ) where

import ECharts.Prelude

import Data.Function.Uncurried (Fn2, runFn2)

import ECharts.Chart (EChart)
import ECharts.Effects (ECHARTS)

newtype Connection =
  Connection (∀ eff. Eff (echarts ∷ ECHARTS |eff) Unit)

foreign import connectImpl
  ∷ ∀ e
  . Fn2 EChart EChart (Eff (echarts ∷ ECHARTS |e) Connection)

connect ∷ ∀ e. EChart → EChart → Eff (echarts ∷ ECHARTS|e) Connection
connect target source = do
  runFn2 connectImpl target source
