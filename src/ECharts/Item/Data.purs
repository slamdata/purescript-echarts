module ECharts.Item.Data where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.StrMap (fromList)

import ECharts.Item.Value
import ECharts.Tooltip
import ECharts.Style.Item


data ItemData = Value ItemValue
              | Dat {
                "value" :: ItemValue,
                "name" :: Maybe String,
                "tooltip" :: Maybe Tooltip,
                "itemStyle" :: Maybe ItemStyle,
                "selected" :: Maybe Boolean
                    }


instance itemDataEncodeJson :: EncodeJson ItemData where
  encodeJson (Value val) = encodeJson val
  encodeJson (Dat conf) =
    fromObject $ fromList $
    [
      "value" := conf.value,
      "name" := conf.name,
      "tooltip" := conf.tooltip,
      "itemStyle" := conf.itemStyle,
      "selected" := conf.selected
    ]

-- Shortcut 
emptyData :: ItemValue -> _
emptyData value =
  {
    "value": value,
    "name": Nothing,
    "tooltip": Nothing,
    "itemStyle": Nothing,
    "selected": Nothing
  }
