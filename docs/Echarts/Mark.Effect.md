## Module ECharts.Mark.Effect

#### `MarkPointEffectRec`

``` purescript
type MarkPointEffectRec = { show :: Maybe Boolean, loop :: Maybe Boolean, period :: Maybe Boolean, scaleSize :: Maybe Boolean, color :: Maybe Color, shadowBlur :: Maybe Number }
```

#### `MarkPointEffect`

``` purescript
newtype MarkPointEffect
  = MarkPointEffect MarkPointEffectRec
```

##### Instances
``` purescript
instance mpEffectEncodeJson :: EncodeJson MarkPointEffect
instance mpEffectDecodeJson :: DecodeJson MarkPointEffect
```

#### `markPointEffectDefault`

``` purescript
markPointEffectDefault :: MarkPointEffectRec
```


