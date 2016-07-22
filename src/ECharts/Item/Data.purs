module ECharts.Item.Data where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Item.Value (ItemValue)
import ECharts.Tooltip (Tooltip)
import ECharts.Style.Item (ItemStyle)

type ItemDataDatRec =
  { value ∷ ItemValue
  , name ∷ Maybe String
  , tooltip ∷ Maybe Tooltip
  , itemStyle ∷ Maybe ItemStyle
  , selected ∷ Maybe Boolean
  }

data ItemData
  = Value ItemValue
  | Dat ItemDataDatRec
  | Label String


instance itemDataEncodeJson ∷ EncodeJson ItemData where
  encodeJson (Value val) = encodeJson val
  encodeJson (Dat conf) =
    encodeJson
      $ SM.fromFoldable
        [ "value" := conf.value
        , "name" := conf.name
        , "tooltip" := conf.tooltip
        , "itemStyle" := conf.itemStyle
        , "selected" := conf.selected
        ]
  encodeJson (Label name) =
    encodeJson $ SM.fromFoldable ["name" := name]

instance itemDataDecodeJson ∷ DecodeJson ItemData where
  decodeJson json =
    (do obj ← decodeJson json
        val ← obj .? "value"
        name ← obj .? "name"
        case name of
          Nothing → pure $ Value val
          Just n → do
            r ← { value: val
                , name: n
                , tooltip: _
                , itemStyle: _
                , selected: _ }
                <$> (obj .? "tooltip")
                <*> (obj .? "itemStyle")
                <*> (obj .? "selected")
            pure $ Dat r)
    <|>
    (Label <$> (decodeJson json >>= (_ .? "name")))
    <|>
    (Value <$> (decodeJson json))


dataDefault ∷ ItemValue → ItemDataDatRec
dataDefault value =
  { value: value
  , name: Nothing
  , tooltip: Nothing
  , itemStyle: Nothing
  , selected: Nothing
  }
