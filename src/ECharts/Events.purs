module ECharts.Events (
  EventType(..),
  EventParam(),
  Sub(),
  listen
  ) where
import Prelude
import Data.Function
import Control.Monad.Eff
import Data.Argonaut.Core

import ECharts.Chart
import ECharts.Effects

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

type EventParam = Json

newtype Sub = Sub (forall eff. Eff (unlisten :: UNLISTEN|eff) Unit)

eventStr :: EventType -> String
eventStr event = case event of
  RefreshEvent -> "refresh"
  RestoreEvent -> "restore"
  ResizeEvent -> "resize"
  ClickEvent -> "click"
  DoubleClickEvent -> "dblclick"
  HoverEvent -> "hover"
  DataChangedEvent -> "dataChanged"
  DataZoomEvent -> "dataZoom"
  DataRangeEvent -> "dataRange"
  DataRangeHoverLinkEvent -> "dataRangeHoverLink"
  LegendSelectedEvent -> "legendSelected"
  LegendHoverLinkEvent -> "legendHoverLink"
  MapSelectedEvent -> "mapSelected"
  PieSelectedEvent -> "pieSelected"
  DataViewChangedEvent -> "dataViewChanged"
  MapRoamEvent -> "mapRoam"
  MagicTypeChangedEvent -> "magicTypeChanged"



foreign import listenImpl :: forall e.
                             Fn3 String
                             (EventParam -> Eff (listen::LISTEN|e) Unit)
                             EChart
                             (Eff (listen::LISTEN|e) Sub)

listen :: forall e.
          EventType ->
          (EventParam -> Eff (listen :: LISTEN|e) Unit) ->
          EChart -> Eff (listen :: LISTEN|e) Sub
listen eventName handler chart = runFn3 listenImpl (eventStr eventName) handler chart
