module ECharts.AddData (
  AdditionalData(..),
  AdditionalDataRec(),
  addData
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import ECharts.Chart
import ECharts.Item.Data
import ECharts.Effects


type AdditionalDataRec = 
  {
    idx :: Number,
    datum :: ItemData,
    isHead :: Boolean,
    dataGrow :: Boolean,
    additionalData :: Maybe String
  }
newtype AdditionalData = AdditionalData AdditionalDataRec


instance additionalDataEncodeJson :: EncodeJson AdditionalData where
  encodeJson (AdditionalData ad) =
    fromArray $
    [
      encodeJson ad.idx,
      encodeJson ad.datum,
      encodeJson ad.isHead,
      encodeJson ad.dataGrow,
      encodeJson ad.additionalData
    ]


foreign import addDataImpl """
function addDataImpl(data, chart) {
  return function() {
    return chart.addData.apply(chart, data);
  };
}
""" :: forall e. Fn2 Json EChart (Eff (dataAdd::ADD_DATA|e) EChart)

addData :: forall e. AdditionalData -> EChart -> Eff (dataAdd::ADD_DATA|e) EChart
addData d chart = runFn2 addDataImpl (encodeJson d) chart

