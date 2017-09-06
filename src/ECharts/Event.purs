module ECharts.Event where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.Maybe as M
import Data.List as L
import Data.Record.Unsafe as R
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant.Internal (RLProxy(..), variantTags, class VariantTags)
import ECharts.Types (EChartsEvent, EChartsEventR, Chart, ECHARTS)
import Type.Row (class RowToList)
import Unsafe.Coerce (unsafeCoerce)

foreign import on_
  ∷ ∀ e
  . Chart
  → String
  → ( Foreign → Eff (echarts ∷ ECHARTS|e) Unit )
  → Eff (echarts ∷ ECHARTS|e) Unit

listenAll
  ∷ ∀ e m
  . MonadEff ( echarts ∷ ECHARTS |e ) m
  ⇒ Chart
  → ( EChartsEvent → Eff (echarts ∷ ECHARTS|e) Unit )
  → m Unit
listenAll chart cb = liftEff $
  for_ eventNames \en → on_ chart en \frn →
    for_ (runExcept $ readProp "type" frn >>= readString) \tp →
      when (tp == en) $ cb $ toEChartsEvent $ Tuple tp frn
  where
  eventNames ∷ ∀ rl. RowToList EChartsEventR rl ⇒ VariantTags rl ⇒ L.List String
  eventNames = variantTags (RLProxy ∷ RLProxy rl)

  toEChartsEvent ∷ ∀ ω. Tuple String ω → EChartsEvent
  toEChartsEvent = unsafeCoerce

foreign import dispatchAction_
  ∷ ∀ e a b action
  . SM.StrMap (Array String) -- mapping from action type to keys that are maybe's
  → ( b → (a → b) → M.Maybe a → b ) -- maybe
  → action
  → Chart
  → Eff ( echarts ∷ ECHARTS |e ) Unit

dispatch
  ∷ ∀ e m
  . MonadEff ( echarts ∷ ECHARTS | e ) m
  ⇒ EChartsEvent
  → Chart
  → m Unit
dispatch vaction chart =
  liftEff $ dispatchAction_ strmapConfig M.maybe action chart
  where
  strmapConfig = SM.fromFoldable
    [ Tuple "highlight" ["name", "dataIndex"]
    , Tuple "downplay" ["name", "dataIndex"]
    , Tuple "mapSelect" ["name", "dataIndex"]
    , Tuple "mapUnSelect" ["name", "dataIndex"]
    , Tuple "mapToggleSelect" ["name", "dataIndex"]
    , Tuple "focusNodeAdjacency" ["seriesId", "seriesIndex", "seriesName"]
    , Tuple "unfocusNodeAdjacency" ["seriesId", "seriesIndex", "seriesName"]
    ]

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
