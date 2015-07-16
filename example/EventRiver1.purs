module EventRiver1 where

import Prelude
import Control.Monad.Eff.Console
import Data.Maybe
import qualified Data.Date as D
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
import ECharts.Common
import ECharts.AddData
import ECharts.Title
import ECharts.Series.EventRiver




options dateDefault = Option $ optionDefault {
  tooltip = Just $ Tooltip tooltipDefault {
     trigger = Just TriggerItem,
     enterable = Just true
     },
  title = Just $ Title titleDefault {
    text = Just "event river",
    subtext = Just "subtext"
    },
  legend = Just $ Legend legendDefault {
    "data" = Just $ legendItemDefault <$> ["first", "second"]
    },
  toolbox = Just $ Toolbox toolboxDefault {
    feature = Just $ Feature featureDefault {
       mark = Just $ MarkFeature $ markFeatureDefault {show = Just true},
       restore = Just $ RestoreFeature $ restoreFeatureDefault {
         show = Just true
         },
       saveAsImage = Just $ SaveAsImageFeature $ saveAsImageFeatureDefault {
         show = Just true
         }
       }
    },
  xAxis = Just $ OneAxis $ Axis axisDefault {
    "type" = Just TimeAxis,
    boundaryGap = Just $ ValueBoundaryGap 0.05 0.1
    },
  series = Just $ Just <$> [
    EventRiverSeries {
       common: universalSeriesDefault {
          name = Just "first"
          },
       eventRiverSeries: eventRiverSeriesDefault {
         weight = Just 123.0,
         eventList = Just [
           OneEvent {
              name: Just "river1",
              weight: Just 123.0,
              evolution: Just [
                Evolution {
                   time: fromMaybe dateDefault $ D.fromString "2014-05-01",
                   value: 14.0,
                   detail: Nothing
                   },
                Evolution {
                  time: fromMaybe dateDefault $ D.fromString "2014-05-02",
                  value: 34.0,
                  detail: Nothing
                  },
                Evolution {
                  time: fromMaybe dateDefault $ D.fromString "2014-05-03",
                  value: 60.0,
                  detail: Nothing
                  }
                ]
              },
           OneEvent {
             name: Just "river2",
             weight: Just 123.0,
             evolution: Just [
               Evolution {
                  time: fromMaybe dateDefault $ D.fromString "2014-05-02",
                  value: 10.0,
                  detail: Nothing
                  },
               Evolution {
                 time: fromMaybe dateDefault $ D.fromString "2014-05-03",
                 value:  34.0,
                 detail: Nothing
                 },
               Evolution {
                 time: fromMaybe dateDefault $ D.fromString "2014-05-05",
                 value:  40.0,
                 detail: Nothing
                 }
               ]
             }
           ]
         }
       },

    EventRiverSeries {
      common: universalSeriesDefault {
         name = Just "second"
         },
      eventRiverSeries: eventRiverSeriesDefault {
        weight = Just 123.0,
        eventList = Just [
          OneEvent {
             name: Just "Apec",
             weight: Just 123.0,
             evolution: Just [
               Evolution {
                  time: fromMaybe dateDefault $ D.fromString "2014-05-06",
                  value: 14.0,
                  detail: Nothing
                  },
               Evolution {
                 time: fromMaybe dateDefault $ D.fromString "2014-05-08",
                 value:  12.0,
                 detail: Nothing
                 },
               Evolution {
                 time: fromMaybe dateDefault $ D.fromString "2014-05-10",
                 value:  14.0,
                 detail: Nothing
                 }
               ]
             }
          ]
        
        }
      }

    ]
  }

                    
eventRiver id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> log "incorrect id in event river"
    Just el -> do
      d <- D.now
      chart <- init Nothing el >>= setOption (options d) true
  
      return unit
