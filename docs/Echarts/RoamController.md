## Module ECharts.RoamController

#### `RoamControllerRec`

``` purescript
type RoamControllerRec = { show :: Maybe Boolean, x :: Maybe XPos, y :: Maybe YPos, width :: Maybe Number, height :: Maybe Number, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), fillerColor :: Maybe Color, handleColor :: Maybe Color, step :: Maybe Number, mapTypeControl :: Maybe (StrMap Boolean) }
```

#### `RoamController`

``` purescript
newtype RoamController
  = RoamController RoamControllerRec
```

##### Instances
``` purescript
instance roamControllerEncodeJson :: EncodeJson RoamController
instance roamControllerDecodeJson :: DecodeJson RoamController
```

#### `roamControllerDefault`

``` purescript
roamControllerDefault :: RoamControllerRec
```


