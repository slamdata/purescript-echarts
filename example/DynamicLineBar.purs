module DynamicLineBar where

import Data.Tuple.Nested
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Maybe
import Data.Array hiding (init)
import Data.String.Regex
import Data.Date
import Data.Traversable
import Data.Foldable
import Utils (precise, getElementById)
import Math (round)

import ECharts.Chart
import ECharts.Options.Unsafe
import ECharts.AddData
import ECharts.Item.Data
import ECharts.Item.Value

import Signal
import Signal.Time (every)


onlyDigRgx :: Regex
onlyDigRgx = regex "^\\D*"  {global: false, ignoreCase: false,
                            multiline: false, sticky: false,
                            unicode: false}

foreign import toLocaleTimeString """
function toLocaleTimeString(date) {
console.log(date);
  return date.toLocaleTimeString();
}
""" :: JSDate -> String

xTimeAxis = do
  curTime <- now
  let start = toEpochMilliseconds curTime
  let mapfn = \i -> fromMaybe "" $ 
                    replace onlyDigRgx  "" <$> 
                    toLocaleTimeString <$>
                    toJSDate <$>
                    fromEpochMilliseconds (start - i * 2000)

  return $ mapfn <$> (1..10)

data2 = do
  let mapfn = \i -> do
        rnd <- random
        return $ precise 1 $  rnd * 10 + 5 
  sequence $ mapfn <$> (1..10)

data1 = do
  let mapfn = \i -> do
        rnd <- random
        return $ round (rnd * 1000)
  sequence $ mapfn <$> (1..10)
  
options_ xAxis d1 d2 = 
  {
    title : {
        text: "动态数据",
        subtext: "纯属虚构"
    },
    tooltip : {
        trigger: "axis"
    },
    legend: {
        data:["最新成交价", "预购队列"]
    },
    toolbox: {
        show : true,
        feature : {
            mark : {show: true},
            dataView : {show: true, readOnly: false},
            magicType : {show: true, type: ["line", "bar"]},
            restore : {show: true},
            saveAsImage : {show: true}
        }
    },
    dataZoom : {
        show : false,
        start : 0,
        end : 100
    },
    xAxis : (
        {
            type : "category",
            boundaryGap : true,
            data: xAxis
        } /\
        {
            type : "category",
            boundaryGap : true,
            data: (1..10)
        }
    ),
    yAxis : (
        {
            type : "value",
            scale: true,
            name : "价格",
            boundaryGap: [0.2, 0.2]
        } /\
        {
            type : "value",
            scale: true,
            name : "预购量",
            boundaryGap: [0.2, 0.2]
        }
    ),
    series : (
        {
            name:"预购队列",
            type:"bar",
            xAxisIndex: 1,
            yAxisIndex: 1,
            data: d1
        } /\
        {
            name:"最新成交价",
            type:"line",
            data: d2
        }
    )
  }


options :: Eff _ _
options = do
  xAxs <- xTimeAxis
  d1 <- data1
  d2 <- data2
  return $ options_ xAxs d1 d2
  

dataStream =
  every 2000 ~> const do
    rnd1 <- random
    rnd2 <- random
    rnd3 <- random
    let lastData = precise 1 $
                   rnd1 * if round ((rnd2 * 10) % 2) == 0 then 1 else -1
    curTime <- now 
    let axisData = 
                   replace onlyDigRgx  "" $ 
                   toLocaleTimeString $ 
                   toJSDate curTime


    let firstData = AdditionalData {
          idx: 0,
          datum: Value $ Simple $ round (rnd3 * 1000),
          isHead: true,
          dataGrow: false,
          additionalData: (Nothing :: Maybe String)
          }
    let sndData = AdditionalData {
          idx: 1,
          datum: Value $ Simple $ lastData,
          isHead: false,
          dataGrow: false,
          additionalData: Just axisData
                  }
    return $ [firstData, sndData]


dynamicLineBar id = do
  opts <- options
  chart <- getElementById id
           >>= init Nothing
           >>= setOptionUnsafe opts true
  runSignal $ dataStream ~> \effContent -> do
    content <- effContent
    sequence_ $ (flip addData) chart <$> content

  
