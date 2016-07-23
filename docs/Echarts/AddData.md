## Module ECharts.AddData

#### `AdditionalDataRec`

``` purescript
type AdditionalDataRec = { idx :: Number, datum :: ItemData, isHead :: Boolean, dataGrow :: Boolean, additionalData :: Maybe String }
```

#### `AdditionalData`

``` purescript
newtype AdditionalData
  = AdditionalData AdditionalDataRec
```

##### Instances
``` purescript
EncodeJson AdditionalData
```

#### `addData`

``` purescript
addData :: forall e. AdditionalData -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```


