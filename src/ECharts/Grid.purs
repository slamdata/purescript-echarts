module ECharts.Grid where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F

import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data GridC
  = Shown Boolean
  | TextStyle T.TextStyle
  | Left T.PixelOrPercent
  | Right T.PixelOrPercent
  | Top T.PixelOrPercent
  | Bottom T.PixelOrPercent

newtype GridM a = GridM (Writer (Array GridC) a)

instance functorGridM ∷ Functor GridM where
  map f (GridM o) = GridM $ map f o

instance applyGridM ∷ Apply GridM where
  apply (GridM f) (GridM o) = GridM $ apply f o

instance applicativeGridM ∷ Applicative GridM where
  pure = GridM <<< pure

instance bindGridM ∷ Bind GridM where
  bind (GridM o) f = GridM $ o >>= (\(GridM o') → o') <<< f

shown ∷ Boolean → GridM Unit
shown = GridM <<< tell <<< Arr.singleton <<< Shown

textStyle ∷ T.TextStyle → GridM Unit
textStyle = GridM <<< tell <<< Arr.singleton <<< TextStyle

left ∷ T.PixelOrPercent → GridM Unit
left = GridM <<< tell <<< Arr.singleton <<< Left

right ∷ T.PixelOrPercent → GridM Unit
right = GridM <<< tell <<< Arr.singleton <<< Right

top ∷ T.PixelOrPercent → GridM Unit
top = GridM <<< tell <<< Arr.singleton <<< Top

bottom ∷ T.PixelOrPercent → GridM Unit
bottom = GridM <<< tell <<< Arr.singleton <<< Bottom

gridTuple ∷ GridC → Tuple String Foreign
gridTuple = case _ of
  Shown o → Tuple "show" $ toForeign $ show o
  TextStyle o → Tuple "textStyle" $ T.unTextStyle o
  Left o → Tuple "left" $ T.pixelOrPercentToForeign o
  Right o → Tuple "right" $ T.pixelOrPercentToForeign o
  Top o → Tuple "top" $ T.pixelOrPercentToForeign o
  Bottom o → Tuple "bottom" $ T.pixelOrPercentToForeign o

buildGrid ∷ GridM Unit → T.Grid
buildGrid (GridM cs) =
  let
    foldFn ∷ GridC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (gridTuple opt)
  in
    T.Grid $ F.foldr foldFn (emptyObject unit) $ execWriter cs
