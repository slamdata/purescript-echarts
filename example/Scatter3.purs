module Scatter3 where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Tuple
import Data.Tuple.Nested
import Data.Array hiding (init)
import Math
import Data.Traversable


import ECharts.Chart
import ECharts.Options
import ECharts.Series
import ECharts.Item.Value
import ECharts.Item.Data
import ECharts.Axis
import Data.Maybe
import qualified Utils as U
showIt = {show: true}


sinData = do 
  randomIs <- U.randomLst 10000
  randomXs <- U.randomLst 10000
  let randoms = zipWith (\i x -> Tuple (U.precise 3 $ i * 10) x)  randomIs randomXs

  let mapfn = \(Tuple i rnd) ->
    Tuple i (U.precise 3 $ sin i - i * (if i % 2 > 0 then 0.1 else -0.1) * rnd)
  return $ mapfn <$> randoms

cosData = do
  randomIs <- U.randomLst 10000
  randomXs <- U.randomLst 10000
  let randoms = zipWith (\i x -> Tuple (U.precise 3 $ i * 10) x)  randomIs randomXs
  let mapfn = \(Tuple i rnd) -> 
        Tuple i (U.precise 3 $ cos i - i * (if i % 2 > 0 then 0.1 else -0.1) * rnd)
  return $ mapfn <$> randoms

simpleData (Tuple a b) = Value $ XYR {
  x: a,
  y: b,
  r: Nothing
  }

options :: Eff _ _ 
options = do
  sines <- sinData
  coses <- cosData
  return $ Option $ optionDefault {
    xAxis = Just $ OneAxis $ Axis axisDefault {"type" = Just ValueAxis},
    yAxis = Just $ OneAxis $ Axis axisDefault {"type" = Just ValueAxis},
    series = Just $ Just <$> [
       ScatterSeries {
          common: universalSeriesDefault{
             "name" = Just "sin"
             },
          "special": scatterSeriesDefault {
            "large" =  Just true,
            "data" = Just $ simpleData <$> sines
            }
          },
       ScatterSeries {
         common: universalSeriesDefault{
            "name" = Just "cos"
            },
         "special": scatterSeriesDefault {
           "large" = Just true,
           "data" = Just $ simpleData <$> coses
           }
         }
       ]
    }
    

scatter3 id = do
  opts <- options
  chart <- U.getElementById id
           >>= init Nothing
           >>= setOption opts true

  return unit

