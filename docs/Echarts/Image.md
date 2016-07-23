## Module ECharts.Image

#### `ImgType`

``` purescript
data ImgType
  = PNG
  | JPEG
```

##### Instances
``` purescript
EncodeJson ImgType
DecodeJson ImgType
```

#### `getDataURL`

``` purescript
getDataURL :: forall e. ImgType -> EChart -> Eff (echarts :: ECHARTS | e) String
```

#### `getImage`

``` purescript
getImage :: forall e. ImgType -> EChart -> Eff (dom :: DOM, echarts :: ECHARTS | e) Node
```


