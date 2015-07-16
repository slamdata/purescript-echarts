## Module ECharts.Grid

#### `GridRec`

``` purescript
type GridRec = { x :: Maybe PercentOrPixel, x2 :: Maybe PercentOrPixel, y :: Maybe PercentOrPixel, y2 :: Maybe PercentOrPixel, width :: Maybe PercentOrPixel, height :: Maybe PercentOrPixel, backgroundColor :: Maybe Color, borderWidth :: Maybe Number, borderColor :: Maybe Number }
```

#### `Grid`

``` purescript
newtype Grid
  = Grid GridRec
```

##### Instances
``` purescript
instance gridEncodeJson :: EncodeJson Grid
instance gridDecodeJson :: DecodeJson Grid
```

#### `gridDefault`

``` purescript
gridDefault :: GridRec
```


