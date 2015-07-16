## Module ECharts.Style.Chord

#### `ChordStyleRec`

``` purescript
type ChordStyleRec = { width :: Maybe Number, color :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Color }
```

#### `ChordStyle`

``` purescript
newtype ChordStyle
  = ChordStyle ChordStyleRec
```

##### Instances
``` purescript
instance chordStyleJson :: EncodeJson ChordStyle
instance chordStyleDecodeJson :: DecodeJson ChordStyle
```

#### `chordStyleDefault`

``` purescript
chordStyleDefault :: ChordStyleRec
```


