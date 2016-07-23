module EventRiver1 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Now (now, NOW)

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.DateTime as DT
import Data.DateTime.Instant as DTI
import Data.Formatter.DateTime as FDT

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

options ∷ DT.DateTime → E.Option
options dateDefault =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault
        { trigger = Just E.TriggerItem
        , enterable = Just true
        }
    , title = Just $ E.Title E.titleDefault
        { text = Just "event river"
        , subtext = Just "subtext"
        }
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault ["first", "second"]
        }
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature E.markFeatureDefault {show = Just true}
            , restore = Just $ E.RestoreFeature E.restoreFeatureDefault
                { show = Just true }
            , saveAsImage = Just $ E.SaveAsImageFeature E.saveAsImageFeatureDefault
                { show = Just true }
            }
        }
    , xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.TimeAxis
        , boundaryGap = Just $ E.ValueBoundaryGap 0.05 0.1
        }
    , series = Just $ map Just
        [ E.EventRiverSeries
            { common: E.universalSeriesDefault { name = Just "first" }
            , eventRiverSeries: E.eventRiverSeriesDefault
                { weight = Just 123.0
                , eventList = Just
                    [ E.OneEvent
                        { name: Just "river1"
                        , weight: Just 123.0
                        , evolution: Just
                            [ E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-01"
                                , value: 14.0
                                , detail: Nothing
                                }
                            , E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-02"
                                , value: 34.0
                                , detail: Nothing
                                }
                            , E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-03"
                                , value: 60.0
                                , detail: Nothing
                                }
                            ]
                        }
                    , E.OneEvent
                        { name: Just "river2"
                        , weight: Just 123.0
                        , evolution: Just
                            [ E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-02"
                                , value: 10.0
                                , detail: Nothing
                                }
                            , E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-03"
                                , value:  34.0
                                , detail: Nothing
                                }
                            , E.Evolution
                                { time:
                                    either (const dateDefault) id
                                    $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-05"
                                , value:  40.0
                                , detail: Nothing
                                }
                            ]
                        }
                    ]
                }
            }
        , E.EventRiverSeries
              { common: E.universalSeriesDefault { name = Just "second" }
              , eventRiverSeries: E.eventRiverSeriesDefault
                  { weight = Just 123.0
                  , eventList = Just
                      [ E.OneEvent
                          { name: Just "Apec"
                          , weight: Just 123.0
                          , evolution: Just
                              [ E.Evolution
                                  { time:
                                      either (const dateDefault) id
                                      $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-06"
                                  , value: 14.0
                                  , detail: Nothing
                                  }
                              , E.Evolution
                                  { time:
                                      either (const dateDefault) id
                                      $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-08"
                                  , value:  12.0
                                  , detail: Nothing
                                  }
                              , E.Evolution
                                  { time:
                                      either (const dateDefault) id
                                      $ FDT.unformatDateTime "YYYY-MM-DD" "2014-05-10"
                                  , value:  14.0
                                  , detail: Nothing
                                  }
                              ]
                          }
                      ]
                  }
              }
        ]
    }

eventRiver
  ∷ ∀ e
  . ElementId
  → Eff (console ∷ CONSOLE, dom ∷ DOM, echarts ∷ E.ECHARTS, now ∷ NOW|e) Unit
eventRiver id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in event river"
    Just el → do
      d ← map DTI.toDateTime now
      chart ← E.init Nothing el >>= E.setOption (options d) true
      pure unit
