module ECharts.XAxis where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data XAxisC
  = Type T.AxisType
  | Items (Array T.Item)
  | AxisTick T.AxisTick
  | AxisLabel T.AxisLabel

newtype XAxisM a = XAxisM (Writer (Array XAxisC) a)

instance functorXAxisM ∷ Functor XAxisM where
  map f (XAxisM o) = XAxisM $ map f o

instance applyXAxisM ∷ Apply XAxisM where
  apply (XAxisM f) (XAxisM w) = XAxisM $ apply f w

instance applicativeXAxisM ∷ Applicative XAxisM where
  pure = XAxisM <<< pure

instance bindXAxisM ∷ Bind XAxisM where
  bind (XAxisM o) f = XAxisM $ o >>= (\(XAxisM o') → o') <<< f

instance monadXAxisM ∷ Monad XAxisM

axisType ∷ T.AxisType → XAxisM Unit
axisType = XAxisM <<< tell <<< Arr.singleton <<< Type

items ∷ Array T.Item → XAxisM Unit
items = XAxisM <<< tell <<< Arr.singleton <<< Items

axisTick ∷ T.AxisTick → XAxisM Unit
axisTick = XAxisM <<< tell <<< Arr.singleton <<< AxisTick

axisLabel ∷ T.AxisLabel → XAxisM Unit
axisLabel = XAxisM <<< tell <<< Arr.singleton <<< AxisLabel

xAxisTuple ∷ XAxisC → Tuple String Foreign
xAxisTuple = case _ of
  Type o → Tuple "type" $ toForeign $ T.printAxisType o
  Items o → Tuple "data" $ toForeign o
  AxisTick o → Tuple "axisTick" $ T.unAxisTick o
  AxisLabel o → Tuple "axisLabel" $ T.unAxisLabel o

buildXAxis ∷ XAxisM Unit → T.XAxis
buildXAxis (XAxisM cs) =
  let
    foldFn ∷ XAxisC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (xAxisTuple opt)
  in
    T.XAxis $ F.foldr foldFn (emptyObject unit) $ execWriter cs
