## Module ECharts.Series.Force

#### `ForceCategoryRec`

``` purescript
type ForceCategoryRec = { name :: Maybe String, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, itemStyle :: Maybe ItemStyle }
```

#### `ForceCategory`

``` purescript
newtype ForceCategory
  = ForceCategory ForceCategoryRec
```

##### Instances
``` purescript
instance forceCategoryEncodeJson :: EncodeJson ForceCategory
instance forceCategoryDecodeJson :: DecodeJson ForceCategory
```

#### `forceCategoryDefault`

``` purescript
forceCategoryDefault :: ForceCategoryRec
```

#### `NodeRec`

``` purescript
type NodeRec = { name :: Maybe String, label :: Maybe String, value :: Number, ignore :: Maybe Boolean, symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, itemStyle :: Maybe ItemStyle, initial :: Maybe (Tuple Number Number), fixX :: Maybe Boolean, fixY :: Maybe Boolean, draggable :: Maybe Boolean, category :: Maybe Number }
```

#### `Node`

``` purescript
newtype Node
  = Node NodeRec
```

##### Instances
``` purescript
instance nodeEncodeJson :: EncodeJson Node
instance nodeDecodeJson :: DecodeJson Node
```

#### `nodeDefault`

``` purescript
nodeDefault :: Number -> NodeRec
```

#### `LinkEnd`

``` purescript
data LinkEnd
  = Name String
  | Index Number
```

##### Instances
``` purescript
instance linkEndEncodeJson :: EncodeJson LinkEnd
instance linkEndDecodeJson :: DecodeJson LinkEnd
```

#### `LinkRec`

``` purescript
type LinkRec = { source :: LinkEnd, target :: LinkEnd, weight :: Number, itemStyle :: Maybe ItemStyle }
```

#### `Link`

``` purescript
newtype Link
  = Link LinkRec
```

##### Instances
``` purescript
instance linkEncodeJson :: EncodeJson Link
instance linkDecodeJson :: DecodeJson Link
```

#### `Matrix`

``` purescript
type Matrix = Array (Array Number)
```


