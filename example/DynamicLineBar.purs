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
import Data.Tuple hiding (zip)


import ECharts.Chart
import ECharts.Events
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Type
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import ECharts.AddData
import ECharts.Title
import qualified ECharts.DataZoom as Zoom


import Signal
import Signal.Time (every)


onlyDigRgx :: Regex
onlyDigRgx = regex "^\\D*"  {global: false, ignoreCase: false,
                            multiline: false, sticky: false,
                            unicode: false}

foreign import toLocaleTimeString """
function toLocaleTimeString(date) {
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

simpleData = Value <<< Simple


options_ xAxis d1 d2 = Option $ optionDefault {
   tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
   legend = Just $ Legend legendDefault {
     "data" = Just $ legendItemDefault <$> ["new price", "pre-order queue"]
     },
   title = Just $ Title titleDefault {
     text = Just "dynamic data",
     subtext = Just "fictitious"
     },
    toolbox = Just $ Toolbox $ toolboxDefault {
     "show" = Just true,
     "feature" = Just $ Feature $ featureDefault {
       "mark" = Just $ MarkFeature $ markFeatureDefault {show = Just true},
       "dataView" = Just $ DataViewFeature $ dataViewFeatureDefault {
         "show" = Just true,
         "readOnly" = Just false
         },
       "magicType" = Just $ MagicTypeFeature $ magicTypeFeatureDefault {
         "show" = Just true,
         "type" = Just [MagicLine, MagicBar]
         },
       "restore" = Just $ RestoreFeature $ restoreFeatureDefault {
         "show" = Just true
         },
       "saveAsImage" = Just $ SaveAsImageFeature $ saveAsImageFeatureDefault {
         "show" = Just true
         }
       }
     },
    "dataZoom" = Just $ Zoom.DataZoom $ Zoom.dataZoomDefault {
      "show" = Just true,
      "start" = Just 0,
      "end" = Just 100
      },
    "xAxis" = Just $ TwoAxises
              (Axis axisDefault {
                  "type" = Just $ CategoryAxis,
                  "boundaryGap" = Just $ CatBoundaryGap true,
                  "data" = Just $ CommonAxisData <$> xAxis
                  })
              (Axis axisDefault {
                  "type" = Just $ CategoryAxis,
                  "boundaryGap" = Just $ CatBoundaryGap true,
                  "data" = Just $ CommonAxisData <$> show <$> (1..10)
                  }),
    "yAxis" = Just $ TwoAxises
              (Axis axisDefault {
                  "type" = Just ValueAxis,
                  "scale" = Just true,
                  "boundaryGap" = Just $ ValueBoundaryGap 0.2 0.2,
                  "name" = Just "price"
                  })
              (Axis axisDefault {
                  "type" = Just ValueAxis,
                  "scale" = Just true,
                  "name" = Just "pre-order quantity",
                  "boundaryGap" = Just $ ValueBoundaryGap 0.2 0.2
                  }),
    "series" = Just $ Just <$> [
      BarSeries {
         common: universalSeriesDefault {
            "name" = Just "pre-order queue"
            },
         special: barSeriesDefault {
           "xAxisIndex" = Just 1,
           "yAxisIndex" = Just 1,
           "data" = Just $ simpleData <$> d1
           }
         },
      LineSeries {
        common: universalSeriesDefault {
           "name" = Just "new price"
           },
        special: lineSeriesDefault {
          "data" = Just $ simpleData <$> d2
          }
        }
      ]
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
           >>= setOption opts true
  runSignal $ dataStream ~> \effContent -> do
    content <- effContent
    sequence_ $ (flip addData) chart <$> content

  
