module Events where

import Math hiding (log)
import Data.Array hiding (init)
import Data.Maybe 

import Control.Monad.Eff
import Control.Monad.Eff.Random

import Utils 

import ECharts.Chart
import ECharts.Events
import ECharts.Options.Unsafe

lineData :: Eff _ [Number]
lineData = do 
  lst <- randomLst 30
  return $ (\x -> round $ x * 30 + 30 ) <$> lst

barData :: Eff _ [Number]
barData = do
  lst <- randomLst 30
  return $ (\x -> round $ x * 10) <$> lst

options_ line bar = {
    tooltip : {
        trigger: "axis"
    },
    legend: {
        data:["最高","最低"]
    },
    toolbox: {
        show : true,
        feature : {
            mark : {show: true},
            dataView : {readOnly:false},
            magicType : {show: true, type: ["line", "bar", "stack", "tiled"]},
            restore : {show: true},
            saveAsImage : {show: true}
        }
    },
    calculable : true,
    dataZoom : {
        show : true,
        realtime : true,
        start : 40,
        end : 60
    },
    xAxis : [
        {
            type : "category",
            boundaryGap : true,
            "data": (\i -> "2013-03-" <> show i) <$> (1..30)
        }
    ],
    yAxis : [
        {
            type : "value"
        }
    ],
    series : [
        {
            name:"最高",
            type:"line",
            "data": line
        },
        {
            name:"最低",
            type:"bar",
            "data": bar
        }
    ]
    }

options :: Eff _ _ 
options = do
  line <- lineData
  bar <- barData
  return $ options_ line bar


subscribe chart = do 
  let sub = \et hndl -> listen et hndl chart
  sub Click log
  sub DoubleClick log
  sub DataZoom log
  sub LegendSelected log
  sub MagicTypeChanged log
  sub DataViewChanged log


events id = do
  opts <- options
  chart <- getElementById id
           >>= init Nothing
           >>= setOptionUnsafe opts true

  subscribe chart
  return unit

