## Module ECharts.Chart

#### `EChart`

``` purescript
data EChart :: *
```

#### `ZRender`

``` purescript
data ZRender :: *
```

#### `Theme`

``` purescript
data Theme
  = ThemeName String
  | ThemeConfig Json
```

##### Instances
``` purescript
instance themeEncodeJson :: EncodeJson Theme
```

#### `init`

``` purescript
init :: forall e. Maybe Theme -> HTMLElement -> Eff (dom :: DOM, echartInit :: ECHARTS_INIT | e) EChart
```

#### `setTheme`

``` purescript
setTheme :: forall e. Theme -> EChart -> Eff (dom :: DOM, echartTheme :: ECHARTS_THEME_SET | e) EChart
```

#### `getZRender`

``` purescript
getZRender :: forall e. EChart -> Eff e ZRender
```

#### `resize`

``` purescript
resize :: forall e. EChart -> Eff (dom :: DOM, echartResize :: ECHARTS_RESIZE | e) Unit
```

#### `refresh`

``` purescript
refresh :: forall e. EChart -> Eff (dom :: DOM, echartRefresh :: ECHARTS_REFRESH | e) Unit
```

#### `clear`

``` purescript
clear :: forall e. EChart -> Eff (dom :: DOM, echartClear :: ECHARTS_CLEAR | e) Unit
```

#### `dispose`

``` purescript
dispose :: forall e. EChart -> Eff (dom :: DOM, echartDispose :: ECHARTS_DISPOSE | e) Unit
```


