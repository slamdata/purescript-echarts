## Module ECharts.Image

#### `ImgType`

``` purescript
data ImgType
  = PNG
  | JPEG
```

##### Instances
``` purescript
instance encodeImg :: EncodeJson ImgType
instance decodeImg :: DecodeJson ImgType
```

#### `getDataURL`

``` purescript
getDataURL :: forall e. ImgType -> EChart -> Eff (image :: IMAGE_MAKING | e) String
```

#### `getImage`

``` purescript
getImage :: forall e. ImgType -> EChart -> Eff (dom :: DOM, image :: IMAGE_MAKING | e) Node
```


