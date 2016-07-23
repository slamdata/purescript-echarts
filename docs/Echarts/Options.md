## Module ECharts.Options

#### `OptionRec`

``` purescript
type OptionRec = { backgroundColor :: Maybe Color, color :: Maybe (Array Color), renderAsImage :: Maybe Boolean, calculable :: Maybe Boolean, animation :: Maybe Boolean, timeline :: Maybe Timeline, tooltip :: Maybe Tooltip, toolbox :: Maybe Toolbox, title :: Maybe Title, legend :: Maybe Legend, dataRange :: Maybe DataRange, dataZoom :: Maybe DataZoom, roamController :: Maybe RoamController, grid :: Maybe Grid, xAxis :: Maybe Axises, yAxis :: Maybe Axises, polar :: Maybe (Array Polar), series :: Maybe (Array (Maybe Series)) }
```

#### `Option`

``` purescript
newtype Option
  = Option OptionRec
```

##### Instances
``` purescript
EncodeJson Option
DecodeJson Option
```

#### `optionDefault`

``` purescript
optionDefault :: OptionRec
```

#### `setOption`

``` purescript
setOption :: forall e. Option -> Boolean -> EChart -> Eff (echarts :: ECHARTS | e) EChart
```


