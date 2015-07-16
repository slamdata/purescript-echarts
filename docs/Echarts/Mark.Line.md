## Module ECharts.Mark.Line

#### `MarkLineRec`

``` purescript
type MarkLineRec = { symbol :: Maybe (Tuple Symbol Symbol), symbolSize :: Maybe DoubleSymbolSize, symbolRotate :: Maybe (Tuple Number Number), effect :: Maybe MarkPointEffect, geoCoord :: Maybe (Array GeoCoord), data :: Maybe (Array (Tuple MarkPointData MarkPointData)), itemStyle :: Maybe ItemStyle }
```

#### `MarkLine`

``` purescript
newtype MarkLine
  = MarkLine MarkLineRec
```

##### Instances
``` purescript
instance mlEncodeJson :: EncodeJson MarkLine
instance mlDecodeJson :: DecodeJson MarkLine
```

#### `markLineDefault`

``` purescript
markLineDefault :: MarkLineRec
```

#### `addMarkLine`

``` purescript
addMarkLine :: forall e a. MarkLine -> EChart -> Eff (addMarkLineECharts :: ADD_MARKLINE | e) EChart
```

#### `delMarkLine`

``` purescript
delMarkLine :: forall e. Number -> String -> EChart -> Eff (removeMarkLine :: REMOVE_MARKLINE | e) EChart
```


