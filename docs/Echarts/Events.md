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
listen :: forall e. EventType -> (EventParam -> Eff (listen :: LISTEN | e) Unit) -> EChart -> Eff (listen :: LISTEN | e) Sub
```


