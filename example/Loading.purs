module Loading where

import Data.Array hiding (init)
import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Random

import Utils
import ECharts.Loading
import ECharts.Style.Text
import ECharts.Chart
import ECharts.Options.Unsafe

import Signal
import Signal.Time

allEffects :: [LoadingEffect]
allEffects = [Spin, Bar, Ring, Whirling, DynamicLine, Bubble]

effect :: LoadingEffect -> LoadingOption
effect eff = LoadingOption $ 
  nullOption{
    text = Just $ show eff,
    effect = Just eff,
    textStyle = Just $ TextStyle $ nullStyle {fontSize = Just 20}
  }

options i =
  let serieType = if i % 2 == 0 then "bar" else "line" in
  {
    tooltip : {
        trigger: "axis"
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
    legend: {
        data:["蒸发量","降水量"]
    },
    xAxis : [
        {
            type : "category",
            data : ["1月","2月","3月","4月",
                    "5月","6月","7月","8月","9月","10月","11月","12月"]
        }
    ],
    yAxis : [
        {
            type : "value"
        }
    ],
    series : [
        {
            name:"蒸发量",
            type: serieType,
            data:[2.0, 4.9, 7.0, 23.2, 25.6, 76.7, 135.6, 162.2, 32.6, 20.0, 6.4, 3.3]
        },
        {
            name:"降水量",
            type: serieType,
            data: [2.6, 5.9, 9.0, 
                   26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
        }
    ]
  }


data ChartSignal a = StartLoading LoadingOption | StopLoading a


dataStream :: Signal (Eff _ (ChartSignal _))
dataStream =
  foldp (\_ curstateE -> do
          curstate <- curstateE
          case curstate of
            StartLoading _ -> do
              eff <- randomInList allEffects
              return $ StopLoading $ options (elemIndex eff allEffects)
            StopLoading _ -> do
              eff <- randomInList allEffects
              return $ StartLoading (effect eff))
  (return $ StartLoading (effect Spin))
  (every 2000)

  
loading id = do
  chart <- getElementById id
           >>= init Nothing

  runSignal $ dataStream ~> \effContent -> do
      content <- effContent
      case content of
        StartLoading loadOptions -> showLoading loadOptions chart
                                    >>= \_ -> return unit
        StopLoading options -> 
          setOptionUnsafe options true chart >>= hideLoading
          >>= \_ -> return unit
