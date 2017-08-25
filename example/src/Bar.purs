module Bar where

import Prelude

import Color as C

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, random)
import DOM (DOM)
import DOM.Node.Types (ElementId(..))
import Data.Array as Arr
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable as F
import Data.Tuple (Tuple(..))
import Data.Variant as V
import Debug.Trace as DT
import ECharts.Chart as EC
import ECharts.Commands as E
import ECharts.Event as EE
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import Utils as U

itemStyle ∷ DSL ETP.ItemStyleI
itemStyle = do
  E.normalItemStyle $ pure unit
  E.emphasisItemStyle do
    E.barBorderWidth 1.0
    E.shadowBlur 10.0
    E.shadowOffsetX 0.0
    E.shadowOffsetY 0.0
    E.shadowColor $ C.rgba 0 0 0 0.5

type OptionInput =
  { label ∷ String
  , one ∷ Number
  , two ∷ Number
  , three ∷ Number
  , four ∷ Number
  }

options ∷ Array OptionInput → DSL ETP.OptionI
options inp = do
  F.for_ (C.fromHexString "#eee") E.backgroundColor

  E.tooltip $ pure unit

  E.legend do
    E.items $ map ET.strItem [ "bar", "bar2", "bar3", "bar4" ]
    E.alignLeft
    E.left $ ET.Pixel 10

  E.brush do
    E.brushToolbox do
      E.rect
      E.polygon
      E.lineX
      E.lineY
      E.keep
      E.clear
    E.xAxisIndex 0

  E.toolbox do
    E.feature do
      E.magicType do
        E.magics do
          E.magicStack
          E.magicTiled
      E.dataView $ pure unit

  E.xAxis do
    E.name "X Axis"
    E.axisLine do
      E.onZero false
    E.splitLine E.hidden
    E.splitArea E.hidden
    E.silent false
    E.items $ map (ET.strItem <<< _.label) inp

  E.yAxis do
    E.inverse true
    E.splitArea E.hidden

  E.grid do
    E.left $ ET.Pixel 100

  E.visualMap do
    E.continuous do
      E.dimension 1
      E.textPair "High" "Low"
      E.inverse true
      E.itemHeight 200.0
      E.calculable true
      E.min (-2.0)
      E.max 6.0
      E.top $ ET.Pixel 60
      E.left $ ET.Pixel 10
      E.inRange do
        E.colorLightness 0.4 0.8
      E.outOfRange do
        F.for_ (C.fromHexString "#bbb") E.color
      E.controller do
        E.inRange do
          F.for_ (C.fromHexString "#2f4554") E.color

  E.series do
    E.bar do
      E.name "bar"
      E.stack "one"
      E.itemStyle itemStyle
      E.items $ map (ET.numItem <<< _.one) inp

    E.bar do
      E.name "bar2"
      E.stack "one"
      E.itemStyle itemStyle
      E.items $ map (ET.numItem <<< _.two) inp

    E.bar do
      E.name "bar3"
      E.stack "two"
      E.itemStyle itemStyle
      E.items $ map (ET.numItem <<< _.three) inp

    E.bar do
      E.name "bar4"
      E.stack "two"
      E.itemStyle itemStyle
      E.items $ map (ET.numItem <<< _.four) inp


genInp ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array OptionInput)
genInp = F.for (Arr.range 0 10) \i → do
  let label = "Class " <> show i
  one ← random <#> ((_ * 2.0) >>> U.precise 2.0)
  two ← random <#> ((_ * -1.0) >>> U.precise 2.0)
  three ← random <#> ((_ * 5.0) >>> U.precise 2.0)
  four ← random <#> ((_ + 0.3) >>> U.precise 2.0)
  pure {label, one, two, three, four}

chart ∷ ∀ e. Eff (random ∷ RANDOM, dom ∷ DOM, echarts ∷ ET.ECHARTS, exception ∷ EXCEPTION|e) Unit
chart = do
  mbEl ← U.getElementById $ ElementId "bar"
  case mbEl of
    Nothing → DT.traceAnyA "There is no element with 'bar' id"
    Just el → do
      ch ← EC.init el
      inp ← genInp
      EC.setOption (options inp)  ch
      EE.listenAll ch DT.traceAnyA
      EE.dispatch (V.inj (SProxy ∷ SProxy "brush") $ toForeign evt) ch
  where
  evt =
    { batch:
      [ { areas:
          [ { brushType: "rect"
            , brushMode: "single"
            , range:
                [ [ 610
                  , 885
                  ]
                , [ 236
                  , 423
                  ]
                ]
            }
          ]
        }
      ]
    }
