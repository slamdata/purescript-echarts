module Funnel where

import Prelude

import Color as C

import Effect (Effect)
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foldable as F
import Data.Maybe (Maybe(..))

-- import Debug.Trace as DT

-- import DOM (DOM)
-- import DOM.Node.Types (ElementId)

import ECharts.Chart as EC
import ECharts.Types as ET
-- import ECharts.Types.Phantom as ETP
import ECharts.Commands as E
import ECharts.Monad (DSL', interpret)

import Utils as U

options ∷ DSL' -- ETP.OptionI
options = do
  E.title do
    E.text "Funnel example"
    E.subtext "This is subtext"

  E.tooltip do
    E.triggerAxis
    E.formatterString "{a} <br />{b}: {c}"

  E.toolbox do
    E.feature do
      E.dataView $ E.readOnly false
      E.restore $ pure unit
      E.saveAsImage $ pure unit

  E.legend
    $ E.items
    $ map ET.strItem [ "one", "two", "three", "four", "five" ]

  E.series do
    E.funnel do
      E.name "Outer"
      E.left $ ET.Percent 10.0
      E.widthPct 80.0
      E.label do
        E.normalLabel do
          E.formatterString "{b} ???"
        E.emphasisLabel do
          E.positionInside
          E.formatterString "{b} ????"
      E.labelLine do
        E.normalLabelLine E.hidden
      E.itemStyle do
        E.normalItemStyle $ E.opacity 0.7

      E.buildItems do
        E.addItem do
          E.name "one"
          E.value 20.0
        E.addItem do
          E.name "two"
          E.value 40.0
        E.addItem do
          E.name "three"
          E.value 60.0
        E.addItem do
          E.name "four"
          E.value 80.0
        E.addItem do
          E.name "five"
          E.value 100.0

    E.funnel do
      E.name "Inner"
      E.left $ ET.Percent 10.0
      E.widthPct 80.0
      E.maxSizePct 80.0

      E.label do
        E.normalLabel do
          E.positionInside
          E.formatterString "{c}%"
          F.for_ (C.fromHexString "#fff") E.color
        E.emphasisLabel do
          E.positionInside
          E.formatterString "??? {c}%"

      E.itemStyle do
        E.normalItemStyle do
          E.opacity 0.5
          F.for_ (C.fromHexString "#fff") E.borderColor
          E.borderWidth 2

      E.buildItems do
        E.addItem do
          E.name "one"
          E.value 5.0
        E.addItem do
          E.name "two"
          E.value 10.0
        E.addItem do
          E.name "three"
          E.value 30.0
        E.addItem do
          E.name "four"
          E.value 50.0
        E.addItem do
          E.name "five"
          E.value 80.0

chart ∷ Effect Unit
chart = do
  mbEl ← U.getElementById "funnel"
  case mbEl of
    Nothing → pure unit --- DT.traceAnyA "There is no element with 'funnel' id"
    Just el → do
      ch ← EC.init el
      EC.setOption (interpret options) ch
