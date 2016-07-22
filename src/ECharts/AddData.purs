module ECharts.AddData
  ( AdditionalData(..)
  , AdditionalDataRec
  , addData
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Argonaut (class Encode, encodeJson)

import ECharts.Chart (EChart)
import ECharts.Item.Data(ItemData)
import ECharts.Effects (ECHARTS)

type AdditionalDataRec =
  { idx ∷ Number
  , datum ∷ ItemData
  , isHead ∷ Boolean
  , dataGrow ∷ Boolean
  , additionalData ∷ Maybe String
  }

newtype AdditionalData =
  AdditionalData AdditionalDataRec


instance additionalDataEncodeJson ∷ EncodeJson AdditionalData where
  encodeJson (AdditionalData ad) =
    fromArray
      [ encodeJson ad.idx
      , encodeJson ad.datum
      , encodeJson ad.isHead
      , encodeJson ad.dataGrow
      , encodeJson ad.additionalData
      ]


foreign import addDataImpl
  ∷ ∀ e. Fn2 Json EChart (Eff (echarts ∷ ECHARTS |e) EChart)

addData ∷ ∀ e. AdditionalData → EChart → Eff (echarts ∷ ECHARTS |e) EChart
addData d chart = runFn2 addDataImpl (encodeJson d) chart
