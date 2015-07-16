## Module ECharts.Toolbox

#### `ToolboxRec`

``` purescript
type ToolboxRec = { show :: Maybe Boolean, orient :: Maybe Orient, x :: Maybe XPos, y :: Maybe YPos, backgroundColor :: Maybe Color, borderColor :: Maybe Color, borderWidth :: Maybe Number, padding :: Maybe (Corner Number), itemGap :: Maybe Number, itemSize :: Maybe Number, color :: Maybe (Array Color), disableColor :: Maybe Color, effectiveColor :: Maybe Color, showTitle :: Maybe Boolean, textStyle :: Maybe TextStyle, feature :: Maybe Feature }
```

#### `Toolbox`

``` purescript
newtype Toolbox
  = Toolbox ToolboxRec
```

##### Instances
``` purescript
instance toolboxEncodeJson :: EncodeJson Toolbox
instance toolboxDecodeJson :: DecodeJson Toolbox
```

#### `toolboxDefault`

``` purescript
toolboxDefault :: ToolboxRec
```

#### `FeatureRec`

``` purescript
type FeatureRec = { mark :: Maybe MarkFeature, dataZoom :: Maybe DataZoomFeature, dataView :: Maybe DataViewFeature, magicType :: Maybe MagicTypeFeature, restore :: Maybe RestoreFeature, saveAsImage :: Maybe SaveAsImageFeature }
```

#### `Feature`

``` purescript
newtype Feature
  = Feature FeatureRec
```

##### Instances
``` purescript
instance featureEncodeJson :: EncodeJson Feature
instance featureDecodeJson :: DecodeJson Feature
```

#### `featureDefault`

``` purescript
featureDefault :: FeatureRec
```

#### `SaveAsImageFeatureRec`

``` purescript
type SaveAsImageFeatureRec = { show :: Maybe Boolean, title :: Maybe String, type :: Maybe ImgType, lang :: Maybe (Array String) }
```

#### `SaveAsImageFeature`

``` purescript
newtype SaveAsImageFeature
  = SaveAsImageFeature SaveAsImageFeatureRec
```

##### Instances
``` purescript
instance saveAsImageEncodeJson :: EncodeJson SaveAsImageFeature
instance saveAsImageDecodeJson :: DecodeJson SaveAsImageFeature
```

#### `saveAsImageFeatureDefault`

``` purescript
saveAsImageFeatureDefault :: SaveAsImageFeatureRec
```

#### `RestoreFeatureRec`

``` purescript
type RestoreFeatureRec = { show :: Maybe Boolean, title :: Maybe String }
```

#### `RestoreFeature`

``` purescript
newtype RestoreFeature
  = RestoreFeature RestoreFeatureRec
```

##### Instances
``` purescript
instance restoreFeatureEncodeJson :: EncodeJson RestoreFeature
instance restoreFeatureDecodeJson :: DecodeJson RestoreFeature
```

#### `restoreFeatureDefault`

``` purescript
restoreFeatureDefault :: RestoreFeatureRec
```

#### `DataZoomFeatureTitleRec`

``` purescript
type DataZoomFeatureTitleRec = { dataZoom :: String, dataZoomReset :: String }
```

#### `DataZoomFeatureTitle`

``` purescript
newtype DataZoomFeatureTitle
  = DataZoomFeatureTitle DataZoomFeatureTitleRec
```

##### Instances
``` purescript
instance datazoomTitleEncodeJson :: EncodeJson DataZoomFeatureTitle
instance datazoomTitleDecodeJson :: DecodeJson DataZoomFeatureTitle
```

#### `DataZoomFeatureRec`

``` purescript
type DataZoomFeatureRec = { show :: Maybe Boolean, title :: Maybe DataZoomFeatureTitle }
```

#### `DataZoomFeature`

``` purescript
newtype DataZoomFeature
  = DataZoomFeature DataZoomFeatureRec
```

##### Instances
``` purescript
instance dataZoomFeatureEncodeJson :: EncodeJson DataZoomFeature
instance dataZoomFeatureDecodeJson :: DecodeJson DataZoomFeature
```

#### `dataZoomFeatureDefault`

``` purescript
dataZoomFeatureDefault :: DataZoomFeatureRec
```

#### `DataViewFeatureRec`

``` purescript
type DataViewFeatureRec = { show :: Maybe Boolean, title :: Maybe String, readOnly :: Maybe Boolean, lang :: Maybe (Array String) }
```

#### `DataViewFeature`

``` purescript
newtype DataViewFeature
  = DataViewFeature DataViewFeatureRec
```

##### Instances
``` purescript
instance dataViewFeatureEncodeJson :: EncodeJson DataViewFeature
instance dataViewFeatureDecodeJson :: DecodeJson DataViewFeature
```

#### `dataViewFeatureDefault`

``` purescript
dataViewFeatureDefault :: DataViewFeatureRec
```

#### `MarkFeatureTitleRec`

``` purescript
type MarkFeatureTitleRec = { mark :: Maybe String, markUndo :: String, markClear :: String }
```

#### `MarkFeatureTitle`

``` purescript
newtype MarkFeatureTitle
  = MarkFeatureTitle MarkFeatureTitleRec
```

##### Instances
``` purescript
instance mftitleEncodeJson :: EncodeJson MarkFeatureTitle
instance mftitleDecodeJson :: DecodeJson MarkFeatureTitle
```

#### `MarkFeatureRec`

``` purescript
type MarkFeatureRec = { show :: Maybe Boolean, title :: Maybe MarkFeatureTitle, lineStyle :: Maybe LineStyle }
```

#### `MarkFeature`

``` purescript
newtype MarkFeature
  = MarkFeature MarkFeatureRec
```

##### Instances
``` purescript
instance markFeatureEncodeJson :: EncodeJson MarkFeature
instance markFeatureDecodeJson :: DecodeJson MarkFeature
```

#### `markFeatureDefault`

``` purescript
markFeatureDefault :: MarkFeatureRec
```

#### `MagicType`

``` purescript
data MagicType
  = MagicLine
  | MagicBar
  | MagicStack
  | MagicTiled
  | MagicForce
  | MagicChord
  | MagicPie
  | MagicFunnel
```

##### Instances
``` purescript
instance magicTypeEncodeJson :: EncodeJson MagicType
instance magicTypeDecodeJson :: DecodeJson MagicType
```

#### `MagicTypeFeatureRec`

``` purescript
type MagicTypeFeatureRec = { show :: Maybe Boolean, title :: Maybe (StrMap String), option :: Maybe Json, type :: Maybe (Array MagicType) }
```

#### `MagicTypeFeature`

``` purescript
newtype MagicTypeFeature
  = MagicTypeFeature MagicTypeFeatureRec
```

##### Instances
``` purescript
instance magicTypeFeatureEncodeJson :: EncodeJson MagicTypeFeature
instance magicTypeFeatureDecodeJson :: DecodeJson MagicTypeFeature
```

#### `magicTypeFeatureDefault`

``` purescript
magicTypeFeatureDefault :: MagicTypeFeatureRec
```


