module ECharts.Tooltip where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F

import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data TooltipC
  = Shown Boolean
  | ShowContent Boolean
  | Trigger T.TooltipTrigger

newtype TooltipM a = TooltipM (Writer (Array TooltipC) a)

instance functorTooltipM ∷ Functor TooltipM where
  map f (TooltipM o) = TooltipM $ map f o

instance applyTooltipM ∷ Apply TooltipM where
  apply (TooltipM f) (TooltipM o) = TooltipM $ apply f o

instance applicativeTooltipM ∷ Applicative TooltipM where
  pure = TooltipM <<< pure

instance bindTooltipM ∷ Bind TooltipM where
  bind (TooltipM o) f = TooltipM $ o >>= (\(TooltipM o') → o') <<< f

instance monadTooltipM ∷ Monad TooltipM

shown ∷ Boolean → TooltipM Unit
shown = TooltipM <<< tell <<< Arr.singleton <<< Shown

showContent ∷ Boolean → TooltipM Unit
showContent = TooltipM <<< tell <<< Arr.singleton <<< ShowContent

trigger ∷ T.TooltipTrigger → TooltipM Unit
trigger = TooltipM <<< tell <<< Arr.singleton <<< Trigger

tooltipTuple ∷ TooltipC → Tuple String Foreign
tooltipTuple = case _ of
  Shown b → Tuple "show" $ toForeign $ show b
  ShowContent b → Tuple "showContent" $ toForeign $ show b
  Trigger t → Tuple "trigger" $ toForeign $ T.printTooltipTrigger t

buildTooltip ∷ TooltipM Unit → T.Tooltip
buildTooltip (TooltipM cs) =
  let
    commands ∷ Array TooltipC
    commands = execWriter cs

    foldFn ∷ TooltipC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (tooltipTuple opt)
  in
    T.Tooltip $ F.foldr foldFn (emptyObject unit) commands
