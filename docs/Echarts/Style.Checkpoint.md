## Module ECharts.Style.Checkpoint

#### `CheckpointStyleRec`

``` purescript
type CheckpointStyleRec = { symbol :: Maybe Symbol, symbolSize :: Maybe SymbolSize, color :: Maybe Color, borderColor :: Maybe Color, label :: Maybe AxisLabel }
```

#### `CheckpointStyle`

``` purescript
newtype CheckpointStyle
  = CheckpointStyle CheckpointStyleRec
```

##### Instances
``` purescript
EncodeJson CheckpointStyle
DecodeJson CheckpointStyle
```

#### `checkpointStyleDefault`

``` purescript
checkpointStyleDefault :: CheckpointStyleRec
```


