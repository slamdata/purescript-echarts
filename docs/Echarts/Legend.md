## Module ECharts.Legend

#### `LegendItemRec`

``` purescript
type LegendItemRec = { icon :: Maybe String, textStyle :: Maybe TextStyle }
```

#### `LegendItem`

``` purescript
data LegendItem
  = LegendItem String LegendItemRec
```

##### Instances
``` purescript
EncodeJson LegendItem
DecodeJson LegendItem
```

#### `legendItemDefault`

``` purescript
legendItemDefault :: String -> LegendItem
```

#### `LegendRec`

``` purescript
type LegendRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemHeight :: Maybe Number, itemWidth :: Maybe Number, textStyle :: Maybe TextStyle, formatter :: Maybe Formatter, selectedMode :: Maybe SelectedMode, selected :: Maybe (StrMap Boolean), data :: Maybe (Array LegendItem) }
```

#### `Legend`

``` purescript
newtype Legend
  = Legend LegendRec
```

##### Instances
``` purescript
EncodeJson Legend
DecodeJson Legend
```

#### `legendDefault`

``` purescript
legendDefault :: LegendRec
```


