module Loading where

import Prelude
import Control.Monad.Eff.Console (print)
import Data.Array hiding (init, zip)
import Data.Maybe
import Data.Tuple 
import Control.Monad.Eff
import Control.Monad.Eff.Random

import Utils
import ECharts.Chart
import ECharts.Events
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import ECharts.AddData
import ECharts.Title
import ECharts.Style.Text
import qualified ECharts.DataZoom as Zoom
import qualified ECharts.Loading as L

import Signal (runSignal, (~>), Signal(), foldp)
import Signal.Time

simpleData = Value <<< Simple

allEffects :: Array L.LoadingEffect
allEffects = [L.Spin, L.Bar, L.Ring, L.Whirling, L.DynamicLine, L.Bubble]

effect :: L.LoadingEffect -> L.LoadingOption
effect eff = L.LoadingOption $ 
  L.loadingOptionDefault{
    text = Just $ "effect",
    effect = Just eff,
    textStyle = Just $ TextStyle $ textStyleDefault {fontSize = Just 20.0}
  }

series true = [
  LineSeries {
     common: universalSeriesDefault {
        name = Just "first"
        },
     lineSeries: lineSeriesDefault {
       "data" = Just $ simpleData <$> [2.0, 4.9, 7.0, 23.2, 25.6,
                                       76.7, 135.6, 162.2, 32.6, 20.0, 6.4, 3.3]
       }
     },
  LineSeries {
    common: universalSeriesDefault {
       name = Just "second"
       },
    lineSeries: lineSeriesDefault {
      "data" = Just $ simpleData <$> [2.6, 5.9, 9.0, 
                                      26.4, 28.7, 70.7, 175.6, 182.2, 48.7,
                                      18.8, 6.0, 2.3]
      }
    }
  ]
series false = [
    BarSeries {
     common: universalSeriesDefault {
        name = Just "first"
        },
     barSeries: barSeriesDefault {
       "data" = Just $ simpleData <$> [2.0, 4.9, 7.0, 23.2, 25.6,
                                       76.7, 135.6, 162.2, 32.6, 20.0, 6.4, 3.3]
       }
     },
  BarSeries {
    common: universalSeriesDefault {
       name = Just "second"
       },
    barSeries: barSeriesDefault {
      "data" = Just $ simpleData <$> [2.6, 5.9, 9.0, 
                                      26.4, 28.7, 70.7, 175.6, 182.2, 48.7,
                                      18.8, 6.0, 2.3]
      }
    }
  ]



options i = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {trigger = Just TriggerAxis},
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just true,
    feature = Just $ Feature $ featureDefault {
      mark = Just $ MarkFeature $ markFeatureDefault {show = Just true},
      dataView = Just $ DataViewFeature $ dataViewFeatureDefault {
        show = Just true,
        readOnly = Just false
        },
      magicType = Just $ MagicTypeFeature $ magicTypeFeatureDefault {
        show = Just true,
        "type" = Just [MagicLine, MagicBar]
        },
      restore = Just $ RestoreFeature $ restoreFeatureDefault {
        show = Just true
        },
      saveAsImage = Just $ SaveAsImageFeature $ saveAsImageFeatureDefault {
        show = Just true
         }
      }
    },
  legend = Just $ Legend legendDefault {
    "data" = Just $ legendItemDefault <$> ["first","second"]
    },
  xAxis = Just $ OneAxis $ Axis axisDefault {
    "type" = Just CategoryAxis,
    "data" = Just $ CommonAxisData <$>
             ["1","2","3","4",
              "5","6","7","8","9","10","11","12"]
    },
  yAxis = Just $ OneAxis $ Axis axisDefault {
    "type" = Just ValueAxis
    },
  series = Just $ Just <$> (series (i `mod` 2.0 == 0.0))
  }

data ChartSignal a = StartLoading L.LoadingOption | StopLoading a


dataStream :: Signal (Eff _ (ChartSignal _))
dataStream =
  foldp (\_ curstateE -> do
          curstate <- curstateE
          case curstate of
            StartLoading _ -> do
              effAndI <- randomInList allEffects
              case effAndI of
                Tuple eff i -> 
                  return $ StopLoading $ options i
            StopLoading _ -> do
              effAndI <- randomInList allEffects
              case effAndI of
                Tuple eff i ->
                  return $ StartLoading (effect eff))
  (return $ StartLoading (effect L.Spin))
  (every 2000.0)

  
loading id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in loading"
    Just el -> do
      chart <- init Nothing el

      runSignal $ dataStream ~> \effContent -> do
        content <- effContent
        case content of
          StartLoading loadOptions -> L.showLoading loadOptions chart
                                      >>= \_ -> return unit
          StopLoading options -> 
            setOption options true chart >>= L.hideLoading
            >>= \_ -> return unit
