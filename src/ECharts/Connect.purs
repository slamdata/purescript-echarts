module ECharts.Connect
  ( connect
  , Connection
  ) where

import Prelude

import Data.Function
import ECharts.Chart
import Control.Monad.Eff
import ECharts.Effects

newtype Connection =
  Connection (∀ eff. Eff (echarts ∷ ECHARTS |eff) Unit)

foreign import connectImpl
  ∷ ∀ e
  . Fn2 EChart EChart (Eff (echarts ∷ ECHARTS |e) Connection)

connect ∷ ∀ e. EChart → EChart → Eff (echarts ∷ ECHARTS|e) Connection
connect target source = do
  runFn2 connectImpl target source
