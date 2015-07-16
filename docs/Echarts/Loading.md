## Module ECharts.Loading

#### `LoadingEffect`

``` purescript
data LoadingEffect
  = Spin
  | Bar
  | Ring
  | Whirling
  | DynamicLine
  | Bubble
```

##### Instances
``` purescript
instance loadingEffectEncodeJson :: EncodeJson LoadingEffect
```

#### `LoadingOptionRec`

``` purescript
type LoadingOptionRec = { text :: Maybe String, x :: Maybe XPos, y :: Maybe YPos, textStyle :: Maybe TextStyle, effect :: Maybe LoadingEffect, effectOption :: Maybe Json, progress :: Maybe Number }
```

#### `LoadingOption`

``` purescript
newtype LoadingOption
  = LoadingOption LoadingOptionRec
```

##### Instances
``` purescript
instance showLoadingOptions :: EncodeJson LoadingOption
```

#### `showLoading`

``` purescript
showLoading :: forall e. LoadingOption -> EChart -> Eff (showLoadingECharts :: LOADING_SHOW | e) EChart
```

#### `hideLoading`

``` purescript
hideLoading :: forall e. EChart -> Eff (hideLoadingECharts :: LOADING_HIDE | e) EChart
```

#### `loadingOptionDefault`

``` purescript
loadingOptionDefault :: LoadingOptionRec
```


