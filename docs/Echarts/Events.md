## Module ECharts.Events

#### `EventType`

``` purescript
data EventType
  = RefreshEvent
  | RestoreEvent
  | ResizeEvent
  | ClickEvent
  | DoubleClickEvent
  | HoverEvent
  | DataChangedEvent
  | DataZoomEvent
  | DataRangeEvent
  | DataRangeHoverLinkEvent
  | LegendSelectedEvent
  | LegendHoverLinkEvent
  | MapSelectedEvent
  | PieSelectedEvent
  | DataViewChangedEvent
  | MapRoamEvent
  | MagicTypeChangedEvent
```

#### `EventParam`

``` purescript
type EventParam = Json
```

#### `Sub`

``` purescript
newtype Sub
```

#### `listen`

``` purescript
listen :: forall e. EventType -> (EventParam -> Eff (echarts :: ECHARTS | e) Unit) -> EChart -> Eff (echarts :: ECHARTS | e) Sub
```


