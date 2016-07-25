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
EncodeJson XPos
DecodeJson XPos
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
EncodeJson YPos
DecodeJson YPos
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
EncodeJson LabelPosition
DecodeJson LabelPosition
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
EncodeJson HorizontalAlign
DecodeJson HorizontalAlign
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
EncodeJson Location
DecodeJson Location
```

#### `Orient`

``` purescript
data Orient
  = Horizontal
  | Vertical
```

##### Instances
``` purescript
EncodeJson Orient
DecodeJson Orient
```


