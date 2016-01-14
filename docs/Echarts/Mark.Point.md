## Module ECharts.Mark.Point

#### `MarkPointRec`

``` purescript
type MarkPointRec = { symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, large :: Maybe Boolean, effect :: Maybe MarkPointEffect, data :: Maybe (Array MarkPointData), geoCoord :: Maybe (StrMap (Tuple Number Number)) }
```

#### `MarkPoint`

``` purescript
newtype MarkPoint
  = MarkPoint MarkPointRec
```

##### Instances
``` purescript
EncodeJson MarkPoint
DecodeJson MarkPoint
```

#### `markPointDefault`

``` purescript
markPointDefault :: MarkPointRec
```

#### `delMarkPoint`

``` purescript
delMarkPoint :: forall e. Number -> String -> EChart -> Eff (removeMarkPointECharts :: REMOVE_MARKPOINT | e) EChart
```

#### `addMarkPoint`

``` purescript
addMarkPoint :: forall e. MarkPoint -> EChart -> Eff (addMarkPointECharts :: ADD_MARKPOINT | e) EChart
```


