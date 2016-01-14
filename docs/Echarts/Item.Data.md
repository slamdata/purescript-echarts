## Module ECharts.Item.Data

#### `ItemDataDatRec`

``` purescript
type ItemDataDatRec = { value :: ItemValue, name :: Maybe String, tooltip :: Maybe Tooltip, itemStyle :: Maybe ItemStyle, selected :: Maybe Boolean }
```

#### `ItemData`

``` purescript
data ItemData
  = Value ItemValue
  | Dat ItemDataDatRec
  | Label String
```

##### Instances
``` purescript
EncodeJson ItemData
DecodeJson ItemData
```

#### `dataDefault`

``` purescript
dataDefault :: ItemValue -> ItemDataDatRec
```


