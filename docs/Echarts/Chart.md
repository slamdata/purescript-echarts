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
EncodeJson Theme
```

#### `init`

``` purescript
init :: forall e. Maybe Theme -> HTMLElement -> Eff (dom :: DOM, echarts :: ECHARTS | e) EChart
```

#### `setTheme`

``` purescript
setTheme :: forall e. Theme -> EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) EChart
```

#### `getZRender`

``` purescript
getZRender :: forall e. EChart -> Eff e ZRender
```

#### `resize`

``` purescript
resize :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `refresh`

``` purescript
refresh :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `clear`

``` purescript
clear :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```

#### `dispose`

``` purescript
dispose :: forall e. EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Unit
```


