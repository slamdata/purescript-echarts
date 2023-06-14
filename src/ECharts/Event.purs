module ECharts.Event (listenAll, dispatch, on_) where

import Prelude (Unit, when, ($), (==), (>>=))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
-- Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import Foreign (Foreign, readString)
import Foreign.Index (readProp)
import Data.List as L
import Record.Unsafe as R
import Data.Tuple (Tuple(..), fst, snd)
import Type.Proxy (Proxy(..))
-- import Data.Variant.Internal (RLProxy(..), variantTags, class VariantTags)
import Data.Variant.Internal (variantTags, class VariantTags)
import ECharts.Types (EChartsEvent, EChartsEventR, Chart)
import Prim.RowList (class RowToList)
-- import Type.Row (class RowToList)
import Unsafe.Coerce (unsafeCoerce)

foreign import on_
  ∷ Chart
  → String
  → ( Foreign → Effect Unit )
  → Effect Unit

listenAll
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → ( EChartsEvent → Effect Unit )
  → m Unit
listenAll chart cb = liftEffect $
  for_ eventNames \en → on_ chart en \frn →
    for_ (runExcept $ readProp "type" frn >>= readString) \tp →
      when (tp == en) $ cb $ toEChartsEvent $ Tuple tp frn
  where
  eventNames ∷ ∀ rl. RowToList EChartsEventR rl ⇒ VariantTags rl ⇒ L.List String
  eventNames = variantTags (Proxy ∷ Proxy rl)

  toEChartsEvent ∷ ∀ ω. Tuple String ω → EChartsEvent
  toEChartsEvent = unsafeCoerce

foreign import dispatchAction_
  ∷ ∀ action
  . action
  → Chart
  → Effect Unit

dispatch
  ∷ ∀ m
  . MonadEffect m
  ⇒ EChartsEvent
  → Chart
  → m Unit
dispatch vaction chart =
  liftEffect $ dispatchAction_ action chart
  where
  variantPair ∷ Tuple String {}
  variantPair = unsafeCoerce vaction

  actionType ∷ String
  actionType = case fst variantPair of
    "legendselectchanged" → "legendToggleSelect"
    "legendselected" → "legendSelect"
    "legendunselected" → "legendUnSelect"
    "datazoom" → "dataZoom"
    "datarangeselected" → "selectDataRange"
    "timelinechanged" → "timelineChange"
    "timelineplaychanged" → "timelinePlayChange"
    "pieselectchanged" → "pieToggleSelect"
    "pieselected" → "pieSelect"
    "pieunselected" → "pieUnSelect"
    "mapselectchanged" → "mapToggleSelect"
    "mapselected" → "mapSelect"
    "mapunselected" → "mapUnSelect"
    "focusnodeadjacency" → "focusNodeAdjacency"
    "unfocusnodeadjacency" → "unfocusNodeAdjacency"
    s → s

  action ∷ ∀ ω. { "type" ∷ String | ω }
  action = R.unsafeSet "type" actionType $ snd variantPair
