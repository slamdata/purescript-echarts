module ECharts.Events
  ( EventType(..)
  , EventParam
  , Sub
  , listen
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

newtype Sub = Sub (∀ eff. Eff (echarts ∷ ECHARTS |eff) Unit)

eventStr ∷ EventType → String
eventStr event = case event of
  RefreshEvent → "refresh"
  RestoreEvent → "restore"
  ResizeEvent → "resize"
  ClickEvent → "click"
  DoubleClickEvent → "dblclick"
  HoverEvent → "hover"
  DataChangedEvent → "dataChanged"
  DataZoomEvent → "dataZoom"
  DataRangeEvent → "dataRange"
  DataRangeHoverLinkEvent → "dataRangeHoverLink"
  LegendSelectedEvent → "legendSelected"
  LegendHoverLinkEvent → "legendHoverLink"
  MapSelectedEvent → "mapSelected"
  PieSelectedEvent → "pieSelected"
  DataViewChangedEvent → "dataViewChanged"
  MapRoamEvent → "mapRoam"
  MagicTypeChangedEvent → "magicTypeChanged"


foreign import listenImpl
  ∷ ∀ e
  . Fn3
      String
      (EventParam → Eff (echarts ∷ ECHARTS |e) Unit)
      EChart
      (Eff (echarts ∷ ECHARTS|e) Sub)

listen
  ∷ ∀ e
  . EventType
  → (EventParam → Eff (echarts ∷ ECHARTS|e) Unit)
  → EChart
  → Eff (echarts ∷ ECHARTS|e) Sub
listen eventName handler chart =
  runFn3 listenImpl (eventStr eventName) handler chart
