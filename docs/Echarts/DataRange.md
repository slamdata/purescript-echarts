## Module ECharts.DataRange

#### `DataRangeRec`

``` purescript
type DataRangeRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemWidth :: Maybe Number, itemHeight :: Maybe Number, min :: Maybe Number, max :: Maybe Number, precision :: Maybe Number, splitNumber :: Maybe Number, selectedMode :: Maybe SelectedMode, calculable :: Maybe Boolean, hoverLink :: Maybe Boolean, realtime :: Maybe Boolean, color :: Maybe (Array Color), formatter :: Maybe Formatter, text :: Maybe (Tuple String String), textStyle :: Maybe TextStyle }
```

#### `DataRange`

``` purescript
newtype DataRange
  = DataRange DataRangeRec
```

##### Instances
``` purescript
EncodeJson DataRange
DecodeJson DataRange
```

#### `dataRangeDefault`

``` purescript
dataRangeDefault :: DataRangeRec
```


