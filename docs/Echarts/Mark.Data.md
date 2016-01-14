## Module ECharts.Mark.Data

#### `MarkPointDataRec`

``` purescript
type MarkPointDataRec = { name :: Maybe String, value :: Maybe Number, x :: Maybe Number, y :: Maybe Number, xAxis :: Maybe Number, yAxis :: Maybe Number, type :: Maybe String }
```

#### `MarkPointData`

``` purescript
newtype MarkPointData
  = MarkPointData MarkPointDataRec
```

##### Instances
``` purescript
EncodeJson MarkPointData
DecodeJson MarkPointData
```

#### `markPointDataDefault`

``` purescript
markPointDataDefault :: MarkPointDataRec
```


