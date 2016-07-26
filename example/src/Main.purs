module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Debug.Trace as DT

import ECharts.Option as E
import ECharts.Tooltip as ET

import Unsafe.Coerce (unsafeCoerce)

foreign import data I ∷ !

main ∷ ∀ e. Eff (i ∷ I|e) Unit
main = do
  DT.traceAnyA $ E.buildOption do
    E.xAxis $ unsafeCoerce unit
    E.yAxis $ unsafeCoerce unit
    E.tooltip do
      ET.shown true
  DT.traceAnyA "!!!"
