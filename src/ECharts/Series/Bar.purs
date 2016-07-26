module ECharts.Series.Bar where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data BarC
  = Name String
  | Items (Array T.Item)
  | Stack String -- ???

newtype BarM a = BarM (Writer (Array BarC) a)

instance functorBarM ∷ Functor BarM where
  map f (BarM o) = BarM $ map f o

instance applyBarM ∷ Apply BarM where
  apply (BarM f) (BarM o) = BarM $ apply f o

instance applicativeBarM ∷ Applicative BarM where
  pure = BarM <<< pure

instance bindBarM ∷ Bind BarM where
  bind (BarM o) f = BarM $ o >>= (\(BarM o') → o') <<< f

instance monadBarM ∷ Monad BarM

name ∷ String → BarM Unit
name = BarM <<< tell <<< Arr.singleton <<< Name

items ∷ Array T.Item → BarM Unit
items = BarM <<< tell <<< Arr.singleton <<< Items

stack ∷ String → BarM Unit
stack = BarM <<< tell <<< Arr.singleton <<< Stack

barTuple ∷ BarC → Tuple String Foreign
barTuple = case _ of
  Name s → Tuple "name" $ toForeign s
  Stack s → Tuple "stack" $ toForeign s
  Items s → Tuple "data" $ toForeign s

buildBar ∷ BarM Unit → T.BarSeries
buildBar (BarM cs) =
  let
    foldFn ∷ BarC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (barTuple opt)
  in
    T.BarSeries $ F.foldr foldFn (emptyObject unit) $ execWriter cs
