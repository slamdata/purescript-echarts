module HeatmapCalendar where

import Prelude

-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)
import Effect (Effect)

import Data.Array as A
import Data.Date as D
import Data.Enum (toEnum)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust)

-- import Debug.Trace as DT

--- import DOM (DOM)
--- import DOM.Node.Types (ElementId)

import ECharts.Chart as EC
import ECharts.Types as ET
-- import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)

import Partial.Unsafe (unsafePartial)
import Utils as U

arrValues ∷ Array (Array String)
arrValues =
  [ [ "2017-01-01", "203" ], [ "2017-01-02", "980" ], [ "2017-01-03", "14" ]
  , [ "2017-01-04", "284" ], [ "2017-01-05", "584" ], [ "2017-01-06", "723" ]
  , [ "2017-01-07", "209" ], [ "2017-01-08", "219" ], [ "2017-01-09", "916" ]
  , [ "2017-01-10", "747" ], [ "2017-01-11", "702" ], [ "2017-01-12", "749" ]
  , [ "2017-01-13", "815" ], [ "2017-01-14", "777" ], [ "2017-01-15", "831" ]
  , [ "2017-01-16", "109" ], [ "2017-01-17", "385" ], [ "2017-01-18", "428" ]
  , [ "2017-01-19", "499" ], [ "2017-01-20", "58" ], [ "2017-01-21", "818" ]
  , [ "2017-01-22", "455" ], [ "2017-01-23", "40" ], [ "2017-01-24", "612" ]
  , [ "2017-01-25", "219" ], [ "2017-01-26", "854" ], [ "2017-01-27", "398" ]
  , [ "2017-01-28", "193" ], [ "2017-01-29", "31" ], [ "2017-01-30", "575" ]
  , [ "2017-01-31", "541" ], [ "2017-02-01", "548" ], [ "2017-02-02", "915" ]
  , [ "2017-02-03", "606" ], [ "2017-02-04", "380" ], [ "2017-02-05", "853" ]
  , [ "2017-02-06", "530" ], [ "2017-02-07", "295" ], [ "2017-02-08", "424" ]
  , [ "2017-02-09", "593" ], [ "2017-02-10", "352" ], [ "2017-02-11", "939" ]
  , [ "2017-02-12", "244" ], [ "2017-02-13", "726" ], [ "2017-02-14", "624" ]
  , [ "2017-02-15", "38" ], [ "2017-02-16", "87" ], [ "2017-02-17", "905" ]
  , [ "2017-02-18", "925" ], [ "2017-02-19", "477" ], [ "2017-02-20", "514" ]
  , [ "2017-02-21", "762" ], [ "2017-02-22", "365" ], [ "2017-02-23", "425" ]
  , [ "2017-02-24", "528" ], [ "2017-02-25", "859" ], [ "2017-02-26", "802" ]
  , [ "2017-02-27", "992" ], [ "2017-02-28", "342" ] ]

values ∷ Array (DSL')
values = A.catMaybes $ arrValues <#> case _ of
  [x, y] → pure $ E.buildValues do
    E.addStringValue x
    E.addStringValue y
  _ → Nothing

options ∷ DSL' -- ETP.OptionI
options = do
  E.tooltip do
    E.positionTop
  E.visualMap $ E.continuous do
    E.min 0.0
    E.max 1000.0
    E.calculable true
    E.orient ET.Horizontal
    E.leftCenter
    E.bottom $ ET.Percent 15.0
  E.calendar do
    E.calendarSpec do
      E.buildRange do
        E.addDateValue $ D.canonicalDate year D.January day
        E.addDateValue $ D.canonicalDate year D.January day'
      E.buildCellSize do
        E.autoValue
        E.addValue 12.0
    E.calendarSpec do
      E.buildRange do
        E.addStringValue "2017-02"
      E.buildCellSize do
        E.autoValue
        E.addValue 12.0
      E.top (ET.Pixel 300)
  E.series do
    E.heatMap do
      E.calendarCoordinateSystem
      E.calendarIndex 0
      E.buildItems
        $ traverse_ E.addItem values
    E.heatMap do
      E.calendarCoordinateSystem
      E.calendarIndex 1
      E.buildItems
        $ traverse_ E.addItem values
  where
    year = unsafePartial $ fromJust <<< toEnum $ 2017
    day = unsafePartial $ fromJust <<< toEnum $ 1
    day' = unsafePartial $ fromJust <<< toEnum $ 31


chart ∷ Effect Unit
chart = do
  mbEl ← U.getElementById "heatmap-calendar"
  case mbEl of
    Nothing → pure unit -- DT.traceAnyA "There is no element with 'heatmap-calendar' id"
    Just el → do
      ch ← EC.init el
      EC.setOption (interpret options) ch
