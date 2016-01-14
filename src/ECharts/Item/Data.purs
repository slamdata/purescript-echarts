module ECharts.Item.Data where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.StrMap (fromList)
import Data.List (toList)

import ECharts.Item.Value
import ECharts.Tooltip
import ECharts.Style.Item

type ItemDataDatRec =  {
  value :: ItemValue,
  name :: Maybe String,
  tooltip :: Maybe Tooltip,
  itemStyle :: Maybe ItemStyle,
  selected :: Maybe Boolean
  }

data ItemData = Value ItemValue
              | Dat ItemDataDatRec
              | Label String


instance itemDataEncodeJson :: EncodeJson ItemData where
  encodeJson (Value val) = encodeJson val
  encodeJson (Dat conf) =
    fromObject $ fromList $ toList
    [
      "value" := conf.value,
      "name" := conf.name,
      "tooltip" := conf.tooltip,
      "itemStyle" := conf.itemStyle,
      "selected" := conf.selected
    ]
  encodeJson (Label name) =
    fromObject $ fromList $ toList
    ["name" := name]

instance itemDataDecodeJson :: DecodeJson ItemData where
  decodeJson json =
    (do obj <- decodeJson json
        val <- obj .? "value"
        name <- obj .? "name"
        case name of
          Nothing -> pure $ Value val
          Just n -> do
            r <- { value: val
                 , name: n
                 , tooltip: _
                 , itemStyle: _
                 , selected: _ } <$>
                 (obj .? "tooltip") <*>
                 (obj .? "itemStyle") <*>
                 (obj .? "selected")
            pure $ Dat r)
    <|>
    (Label <$> (decodeJson json >>= (.? "name")))
    <|>
    (Value <$> (decodeJson json))



dataDefault :: ItemValue -> ItemDataDatRec
dataDefault value =
  {
    value: value,
    name: Nothing,
    tooltip: Nothing,
    itemStyle: Nothing,
    selected: Nothing
  }
