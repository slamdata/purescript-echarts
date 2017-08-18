module ECharts.Event where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Index (readProp)
import Data.List as L
import Data.Tuple (Tuple(..))
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
  ∷ ∀ e
  . Chart
  → ( EChartsEvent → Eff (echarts ∷ ECHARTS|e) Unit )
  → Eff (echarts ∷ ECHARTS|e) Unit
listenAll chart cb =
  for_ eventNames \en → on_ chart en \frn →
    for_ (runExcept $ readProp "type" frn >>= readString) \tp →
      when (tp == en) $ cb $ toEChartsEvent $ Tuple tp frn
  where
  eventNames ∷ ∀ rl. RowToList EChartsEventR rl ⇒ VariantTags rl ⇒ L.List String
  eventNames = variantTags (RLProxy ∷ RLProxy rl)

  toEChartsEvent ∷ ∀ ω. Tuple String ω → EChartsEvent
  toEChartsEvent = unsafeCoerce
