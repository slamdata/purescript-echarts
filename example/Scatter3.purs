module Scatter3 where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Tuple
import Data.Tuple.Nested
import Data.Array hiding (init)
import Math
import Data.Traversable


import ECharts.Chart
import ECharts.Options.Unsafe
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

options :: Eff _ _ 
options = do

  sines <- sinData
  coses <- cosData
  return
    {
      tooltip:
      {
        trigger: "axis",
        showDelay: 0,
        axisPointer:
        {
          "type": "cross",
          lineStyle:
          {
            "type": "dashed",
            width: 1
          }
        }
      },
      legend: {"data": ["sin", "cos"]},
    
      toolbox:
      {
        show: true,
        feature:
        {
          mark: showIt,
          dataZoom: showIt,
          dataView: {show: true, readOnly: true},
          restore: showIt,
          saveAsImage: showIt
        }
      },
      xAxis: [{"type": "value", scale: true}],
      yAxis: [{"type": "value", scale: true}],
      series:
      (
         {
           name: "sin",
           "type": "scatter",
           large: true,
           "data": sines
         } /\
         {
           name: "cos",
           "type": "scatter",
           large: true,
           "data": coses
         }
      )
    }
    

scatter3 id = do
  opts <- options
  chart <- U.getElementById id
           >>= init Nothing
           >>= setOptionUnsafe opts true

  return unit

