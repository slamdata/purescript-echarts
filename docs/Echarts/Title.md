## Module ECharts.Title

#### `LinkTarget`

``` purescript
data LinkTarget
  = Self
  | Blank
```

##### Instances
``` purescript
EncodeJson LinkTarget
DecodeJson LinkTarget
```

#### `TitleRec`

``` purescript
type TitleRec = { text :: Maybe String, link :: Maybe String, target :: Maybe LinkTarget, subtext :: Maybe String, sublink :: Maybe String, subtarget :: Maybe LinkTarget, x :: Maybe XPos, y :: Maybe YPos, textAlign :: Maybe HorizontalAlign, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, textStyle :: Maybe TextStyle, subtextStyle :: Maybe TextStyle }
```

#### `Title`

``` purescript
newtype Title
  = Title TitleRec
```

##### Instances
``` purescript
EncodeJson Title
DecodeJson Title
```

#### `titleDefault`

``` purescript
titleDefault :: TitleRec
```


