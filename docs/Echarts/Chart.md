## Module ECharts.Chart

#### `init`

``` purescript
init :: forall m e. MonadEff (dom :: DOM, echarts :: ECHARTS, err :: EXCEPTION | e) m => HTMLElement -> m Chart
```

#### `setOption`

``` purescript
setOption :: forall m e. MonadEff (echarts :: ECHARTS, err :: EXCEPTION | e) m => DSL OptionI -> Chart -> m Unit
```

#### `resetOption`

``` purescript
resetOption :: forall m e. MonadEff (echarts :: ECHARTS, err :: EXCEPTION | e) m => DSL OptionI -> Chart -> m Unit
```

#### `resize`

``` purescript
resize :: forall m e. MonadEff (echarts :: ECHARTS | e) m => Chart -> m Unit
```

#### `clear`

``` purescript
clear :: forall m e. MonadEff (echarts :: ECHARTS | e) m => Chart -> m Unit
```

#### `dispose`

``` purescript
dispose :: forall m e. MonadEff (echarts :: ECHARTS | e) m => Chart -> m Unit
```


