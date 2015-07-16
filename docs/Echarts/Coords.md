## Module ECharts.Coords

#### `XPos`

``` purescript
data XPos
  = XLeft
  | XRight
  | XCenter
  | X Number
```

##### Instances
``` purescript
instance xPosEncodeJson :: EncodeJson XPos
instance xPosDecodeJson :: DecodeJson XPos
```

#### `YPos`

``` purescript
data YPos
  = YTop
  | YBottom
  | YCenter
  | Y Number
```

##### Instances
``` purescript
instance yPosEncodeJson :: EncodeJson YPos
instance yPosDecodeJson :: DecodeJson YPos
```

#### `LabelPosition`

``` purescript
data LabelPosition
  = LPOuter
  | LPInner
  | LPTop
  | LPRight
  | LPLeft
  | LPBottom
  | LPInside
  | LPInsideLeft
  | LPInsideRight
  | LPInsideTop
  | LPInsideBottom
```

##### Instances
``` purescript
instance labelPositionEncodeJson :: EncodeJson LabelPosition
instance labelPositionDecodeJson :: DecodeJson LabelPosition
```

#### `HorizontalAlign`

``` purescript
data HorizontalAlign
  = HAlignLeft
  | HAlignRight
  | HAlignCenter
```

##### Instances
``` purescript
instance textAlignEncodeJson :: EncodeJson HorizontalAlign
instance textAlignDecodeJson :: DecodeJson HorizontalAlign
```

#### `LocationRec`

``` purescript
type LocationRec = { x :: Maybe XPos, y :: Maybe YPos }
```

#### `Location`

``` purescript
newtype Location
  = Location LocationRec
```

##### Instances
``` purescript
instance locationEncodeJson :: EncodeJson Location
instance locationDecodeJson :: DecodeJson Location
```

#### `Orient`

``` purescript
data Orient
  = Horizontal
  | Vertical
```

##### Instances
``` purescript
instance orientEncodeJson :: EncodeJson Orient
instance orientDecodeJson :: DecodeJson Orient
```


