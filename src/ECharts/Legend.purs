module ECharts.Legend where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F

import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)


data LegendC
  = Orient T.Orient
  | Items (Array T.Item)
  | Shown Boolean
  | Left T.PixelOrPercent
  | Right T.PixelOrPercent
  | Top T.PixelOrPercent
  | Bottom T.PixelOrPercent

newtype LegendM a = LegendM (Writer (Array LegendC) a)

instance functorLegendM ∷ Functor LegendM where
  map f (LegendM o) = LegendM $ map f o

instance applyLegendM ∷ Apply LegendM where
  apply (LegendM f) (LegendM o) = LegendM $ apply f o

instance applicativeLegendM ∷ Applicative LegendM where
  pure = LegendM <<< pure

instance bindLegendM ∷ Bind LegendM where
  bind (LegendM o) f = LegendM $ o >>= (\(LegendM o') → o') <<< f

instance monadLegendM ∷ Monad LegendM

orient ∷ T.Orient → LegendM Unit
orient = LegendM <<< tell <<< Arr.singleton <<< Orient

shown ∷ Boolean → LegendM Unit
shown = LegendM <<< tell <<< Arr.singleton <<< Shown

items ∷ Array T.Item → LegendM Unit
items = LegendM <<< tell <<< Arr.singleton <<< Items

left ∷ T.PixelOrPercent → LegendM Unit
left = LegendM <<< tell <<< Arr.singleton <<< Left

right ∷ T.PixelOrPercent → LegendM Unit
right = LegendM <<< tell <<< Arr.singleton <<< Right

top ∷ T.PixelOrPercent → LegendM Unit
top = LegendM <<< tell <<< Arr.singleton <<< Top

bottom ∷ T.PixelOrPercent → LegendM Unit
bottom = LegendM <<< tell <<< Arr.singleton <<< Bottom


legendTuple ∷ LegendC → Tuple String Foreign
legendTuple = case _ of
  Orient f → Tuple "orient" $ toForeign $ T.printOrient f
  Shown f → Tuple "show" $ toForeign $ show f
  Items f → Tuple "data" $ toForeign f
  Left f → Tuple "left" $ T.pixelOrPercentToForeign f
  Right f → Tuple "right" $ T.pixelOrPercentToForeign f
  Top f → Tuple "top" $ T.pixelOrPercentToForeign f
  Bottom f → Tuple "bottom" $ T.pixelOrPercentToForeign f

buildLegend ∷ LegendM Unit → T.Legend
buildLegend (LegendM cs) =
  let
    foldFn ∷ LegendC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (legendTuple opt)
  in
    T.Legend $ F.foldr foldFn (emptyObject unit) $ execWriter cs
