## Module ECharts.Tooltip

#### `TooltipTrigger`

``` purescript
data TooltipTrigger
  = TriggerItem
  | TriggerAxis
```

##### Instances
``` purescript
instance tooltipTriggerEncodeJson :: EncodeJson TooltipTrigger
instance tooltipTriggerDecodeJson :: DecodeJson TooltipTrigger
```

#### `TooltipPosition`

``` purescript
data TooltipPosition
  = Fixed (Array Number)
  | FuncPos (Array Number -> Array Number)
```

##### Instances
``` purescript
instance tooltipPositionEncodeJson :: EncodeJson TooltipPosition
instance tooltipPositionDecodeJson :: DecodeJson TooltipPosition
```

#### `TooltipAxisPointerType`

``` purescript
data TooltipAxisPointerType
  = LinePointer
  | CrossPointer
  | ShadowPointer
  | NonePointer
```

##### Instances
``` purescript
instance tooltipAxisPointerTypeEncodeJson :: EncodeJson TooltipAxisPointerType
instance tooltiopAxisPointerTypeDecodeJson :: DecodeJson TooltipAxisPointerType
```

#### `TooltipAxisPointerRec`

``` purescript
type TooltipAxisPointerRec = { type :: Maybe TooltipAxisPointerType, lineStyle :: Maybe LineStyle, crossStyle :: Maybe LineStyle, shadowStyle :: Maybe AreaStyle }
```

#### `TooltipAxisPointer`

``` purescript
newtype TooltipAxisPointer
  = TooltipAxisPointer TooltipAxisPointerRec
```

##### Instances
``` purescript
instance tooltipAxisPointerEncodeJson :: EncodeJson TooltipAxisPointer
instance tooltipAxisPointerDecodeJson :: DecodeJson TooltipAxisPointer
```

#### `tooltipAxisPointerDefault`

``` purescript
tooltipAxisPointerDefault :: TooltipAxisPointerRec
```

#### `TooltipRec`

``` purescript
type TooltipRec = { show :: Maybe Boolean, showContent :: Maybe Boolean, trigger :: Maybe TooltipTrigger, position :: Maybe TooltipPosition, formatter :: Maybe Formatter, islandFormatter :: Maybe Formatter, showDelay :: Maybe Number, hideDelay :: Maybe Number, transitionDuration :: Maybe Number, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderRadius :: Maybe Number, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), axisPointer :: Maybe TooltipAxisPointer, textStyle :: Maybe TextStyle, enterable :: Maybe Boolean }
```

#### `Tooltip`

``` purescript
newtype Tooltip
  = Tooltip TooltipRec
```

##### Instances
``` purescript
instance tooltipEncodeJson :: EncodeJson Tooltip
instance tooltipDecodeJson :: DecodeJson Tooltip
```

#### `tooltipDefault`

``` purescript
tooltipDefault :: TooltipRec
```


