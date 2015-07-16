## Module ECharts.Item.Value

#### `XYRRec`

``` purescript
type XYRRec = { x :: Number, y :: Number, r :: Maybe Number }
```

#### `HLOCRec`

``` purescript
type HLOCRec = { h :: Number, l :: Number, o :: Number, c :: Number }
```

#### `ItemValue`

``` purescript
data ItemValue
  = None
  | Simple Number
  | Many (Array Number)
  | XYR XYRRec
  | HLOC HLOCRec
```

##### Instances
``` purescript
instance itemValueEncodeJson :: EncodeJson ItemValue
instance itemValueDecodeJson :: DecodeJson ItemValue
```


