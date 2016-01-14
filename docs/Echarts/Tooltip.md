## Module ECharts.Tooltip

#### `TooltipTrigger`

``` purescript
data TooltipTrigger
  = TriggerItem
  | TriggerAxis
```

##### Instances
``` purescript
EncodeJson TooltipTrigger
DecodeJson TooltipTrigger
```

#### `TooltipPosition`

``` purescript
data TooltipPosition
  = Fixed (Array Number)
  | FuncPos (Array Number -> Array Number)
```

##### Instances
``` purescript
EncodeJson TooltipPosition
DecodeJson TooltipPosition
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
EncodeJson TooltipAxisPointerType
DecodeJson TooltipAxisPointerType
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
EncodeJson TooltipAxisPointer
DecodeJson TooltipAxisPointer
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
EncodeJson Tooltip
DecodeJson Tooltip
```

#### `tooltipDefault`

``` purescript
tooltipDefault :: TooltipRec
```


