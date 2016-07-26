module ECharts.YAxis where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)


data YAxisC
  = Type T.AxisType
  | Items (Array T.Item)
  | AxisTick T.AxisTick
  | AxisLabel T.AxisLabel

newtype YAxisM a = YAxisM (Writer (Array YAxisC) a)

instance functorYAxisM ∷ Functor YAxisM where
  map f (YAxisM o) = YAxisM $ map f o

instance applyYAxisM ∷ Apply YAxisM where
  apply (YAxisM f) (YAxisM w) = YAxisM $ apply f w

instance applicativeYAxisM ∷ Applicative YAxisM where
  pure = YAxisM <<< pure

instance bindYAxisM ∷ Bind YAxisM where
  bind (YAxisM o) f = YAxisM $ o >>= (\(YAxisM o') → o') <<< f

instance monadYAxisM ∷ Monad YAxisM

axisType ∷ T.AxisType → YAxisM Unit
axisType = YAxisM <<< tell <<< Arr.singleton <<< Type

items ∷ Array T.Item → YAxisM Unit
items = YAxisM <<< tell <<< Arr.singleton <<< Items

axisTick ∷ T.AxisTick → YAxisM Unit
axisTick = YAxisM <<< tell <<< Arr.singleton <<< AxisTick

axisLabel ∷ T.AxisLabel → YAxisM Unit
axisLabel = YAxisM <<< tell <<< Arr.singleton <<< AxisLabel

yAxisTuple ∷ YAxisC → Tuple String Foreign
yAxisTuple = case _ of
  Type o → Tuple "type" $ toForeign $ T.printAxisType o
  Items o → Tuple "data" $ toForeign o
  AxisTick o → Tuple "axisTick" $ T.unAxisTick o
  AxisLabel o → Tuple "axisLabel" $ T.unAxisLabel o

buildYAxis ∷ YAxisM Unit → T.YAxis
buildYAxis (YAxisM cs) =
  let
    foldFn ∷ YAxisC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (yAxisTuple opt)
  in
    T.YAxis $ F.foldr foldFn (emptyObject unit) $ execWriter cs
