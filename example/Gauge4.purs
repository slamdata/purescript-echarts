module Gauge4 where

import Control.Monad.Eff
import Control.Monad.Eff.Random
--import Data.Tuple
import Data.Tuple.Nested
import Signal
import Signal.Time
import ECharts.Chart
import ECharts.Options.Unsafe
import Data.Maybe
import Utils
import Utils

type GaugeValue =
  {
    first :: Number,
    second :: Number,
    third :: Number,
    fourth :: Number 
  }

gaugeValueSignal =
  every 2000 ~> const do
    fst <- random
    snd <- random
    thd <- random
    fth <- random
    return {
      first: precise 2 $ fst * 100,
      second: precise 2 $ snd * 7,
      third: precise 2 $ thd * 2,
      fourth: precise 2 $ fth * 2
      }

options_ :: forall e. GaugeValue -> _
options_ gaugeValue =
  {
    tooltip : {
        formatter: "{a} <br/>{c} {b}"
    },
    toolbox: {
        show : true,
        feature : {
            mark : {show: true},
            restore : {show: true},
            saveAsImage : {show: true}
        }
    },
    series : (
        {
            name:"速度",
            type:"gauge",
            min:0,
            max:220,
            splitNumber:11,
            axisLine: {            
                lineStyle: {       
                    width: 10
                }
            },
            axisTick: {            
                length :15,        
                lineStyle: {       
                    color: "auto"
                }
            },
            splitLine: {           
                length :20,        
                lineStyle: {       
                    color: "auto"
                }
            },
            title : {
                textStyle: {       
                    fontWeight: "bolder",
                    fontSize: 20,
                    fontStyle: "italic"
                }
            },
            detail : {
                textStyle: {       
                    fontWeight: "bolder"
                }
            },
            data:[{value: gaugeValue.first, name: "km/h"}]
        }/\
        {
            name:"转速",
            type:"gauge",
            center : ["25%", "55%"], 
            radius : "50%",
            min:0,
            max:7,
            endAngle:45,
            splitNumber:7,
            axisLine: {            
                lineStyle: {       
                    width: 8
                }
            },
            axisTick: {            
                length :12,        
                lineStyle: {       
                    color: "auto"
                }
            },
            splitLine: {           
                length :20,        
                lineStyle: {       
                    color: "auto"
                }
            },
            pointer: {
                width:5
            },
            title : {
                offsetCenter: (0 /\ "-30%")
            },
            detail : {
                textStyle: {       
                    fontWeight: "bolder"
                }
            },
            data:[{value: gaugeValue.second, name: "x1000 r/min"}]
        } /\
        {
            name:"油表",
            type:"gauge",
            center : ["75%", "50%"], 
            radius : "50%",
            min:0,
            max:2,
            startAngle:135,
            endAngle:45,
            splitNumber:2,
            axisLine: {           
                lineStyle: {      
                    color: [(0.2 /\ "#ff4500"),(0.8 /\ "#48b"),(1 /\ "#228b22")], 
                    width: 8
                }
            },
            axisTick: {           
                splitNumber:5,
                length :10,       
                lineStyle: {      
                    color: "auto"
                }
            },
            axisLabel: {
                formatter: \v -> case v of
                   0 -> "E"
                   1 -> "Gas"
                   2 -> "F"
                   _ -> "" 

            },
            splitLine: {         
                length :15,      
                lineStyle: {     
                    color: "auto"
                }
            },
            pointer: {
                width:2
            },
            title : {
                show: false
            },
            detail : {
                show: false
            },
            data:[{value: gaugeValue.third, name: "gas"}]
        } /\
        {
            name:"水表",
            type:"gauge",
            center : ["75%", "50%"],
            radius : "50%",
            min:0,
            max:2,
            startAngle:315,
            endAngle:225,
            splitNumber:2,
            axisLine: {   
                lineStyle: { 
                    color: [(0.2 /\ "#ff4500"), (0.8 /\ "#48b"), (1 /\ "#228b22")], 
                    width: 8
                }
            },
            axisTick: {      
                show: false
            },
            axisLabel: {
                formatter: \v -> case v of
                               0 -> "H"
                               1 -> "Water"
                               2 -> "C"
                               _ -> ""
            },
            splitLine: {     
                length :15,  
                lineStyle: { 
                    color: "auto"
                }
            },
            pointer: {
                width:2
            },
            title : {
                show: false
            },
            detail : {
                show: false
            },
            data:[{value: gaugeValue.fourth, name: "gas"}]
        }
        )
  }

options = gaugeValueSignal ~> \g -> do
  gv <- g
  return $ options_ gv

gauge4 id = do
  chart <- getElementById id
           >>= init Nothing
  runSignal $ options ~> \opts -> do
    os <- opts
    setOptionUnsafe os true chart
    return unit
           
