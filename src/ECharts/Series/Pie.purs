module ECharts.Series.Pie where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data PieC
  = Name String
  | Center T.Point
  | Radius T.Radius
  | Items (Array T.Item)
  | StartAngle Number

newtype PieM a = PieM (Writer (Array PieC) a)

instance functorPieM ∷ Functor PieM where
  map f (PieM o) = PieM $ map f o

instance applyPieM ∷ Apply PieM where
  apply (PieM f) (PieM o) = PieM $ apply f o

instance applicativePieM ∷ Applicative PieM where
  pure = PieM <<< pure

instance bindPieM ∷ Bind PieM where
  bind (PieM o) f = PieM $ o >>= (\(PieM o') → o') <<< f

instance monadPieM ∷ Monad PieM

name ∷ String → PieM Unit
name = PieM <<< tell <<< Arr.singleton <<< Name

center ∷ T.Point → PieM Unit
center = PieM <<< tell <<< Arr.singleton <<< Center

radius ∷ T.Radius → PieM Unit
radius = PieM <<< tell <<< Arr.singleton <<< Radius

items ∷ Array T.Item → PieM Unit
items = PieM <<< tell <<< Arr.singleton <<< Items

startAngle ∷ Number → PieM Unit
startAngle = PieM <<< tell <<< Arr.singleton <<< StartAngle

pieTuple ∷ PieC → Tuple String Foreign
pieTuple = case _ of
  Name s → Tuple "name" $ toForeign s
  Center c → Tuple "center" $ T.pointToForeign c
  Radius r → Tuple "radius" $ T.radiusToForeign r
  Items is → Tuple "data" $ toForeign is
  StartAngle n → Tuple "startAngle" $ toForeign n

buildPie ∷ PieM Unit → T.PieSeries
buildPie (PieM cs) =
  let
    foldFn ∷ PieC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (pieTuple opt)
  in
    T.PieSeries $ F.foldr foldFn (emptyObject unit) $ execWriter cs
