module Scatter3 where

import Prelude

import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Array (zipWith)
import Data.NonEmpty as NE

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Math (cos, sin, (%))

import Utils as U

showIt ∷ {show ∷ Boolean}
showIt = {show: true}

sinData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array (Tuple Number Number))
sinData = do
  randomIs ← map NE.oneOf $ U.randomArray 10000
  randomXs ← map NE.oneOf $ U.randomArray 10000
  let
    randoms =
      zipWith (\i x → Tuple (U.precise 3.0 $ i * 10.0) x)  randomIs randomXs
    mapfn (Tuple i rnd) =
      Tuple i (U.precise 3.0 $ sin i - i * (if i `mod` 2.0 > 0.0 then 0.1 else -0.1) * rnd)
  pure $ map mapfn randoms

cosData ∷ ∀ e. Eff (random ∷ RANDOM|e) (Array (Tuple Number Number))
cosData = do
  randomIs ← map NE.oneOf $ U.randomArray 10000
  randomXs ← map NE.oneOf $ U.randomArray 10000
  let
    randoms =
      zipWith (\i x → Tuple (U.precise 3.0 $ i * 10.0) x)  randomIs randomXs
    mapfn (Tuple i rnd) =
      Tuple i (U.precise 3.0 $ cos i - i * (if i % 2.0 > 0.0 then 0.1 else -0.1) * rnd)
  pure $ map mapfn randoms

simpleData ∷ Tuple Number Number → E.ItemData
simpleData (Tuple x y) =
  E.Value $ E.XYR { x, y, r: Nothing }

options ∷ ∀ e. Eff (random ∷ RANDOM|e) E.Option
options = do
  sines ← sinData
  coses ← cosData
  pure
    $ E.Option
    $ E.optionDefault
        { xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault {"type" = Just E.ValueAxis}
        , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault {"type" = Just E.ValueAxis}
        , series = Just $ map Just
            [ E.ScatterSeries
                { common: E.universalSeriesDefault
                    { name = Just "sin" }

                , scatterSeries: E.scatterSeriesDefault
                    { large =  Just true
                    , "data" = Just $ map simpleData sines
                    }
                }
            , E.ScatterSeries
                { common: E.universalSeriesDefault
                    { name = Just "cos" }
                , scatterSeries: E.scatterSeriesDefault
                    { large = Just true
                    , "data" = Just $ map simpleData coses
                    }
                }
            ]
        }

scatter3
  ∷ ∀ e
  . ElementId
  → Eff (dom ∷ DOM, console ∷ CONSOLE, random ∷ RANDOM, echarts ∷ E.ECHARTS|e) Unit
scatter3 id = do
  mbEl ← U.getElementById id
  case mbEl  of
    Nothing → log "incorrect id in scatter3"
    Just el → do
      opts ← options
      E.init Nothing el >>= E.setOption opts true
      pure unit
