module Mix2Safe where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

series ∷ Array E.Series
series =
  [ E.BarSeries
      { common: E.universalSeriesDefault {name = Just "direct access"}
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0]
          , stack = Just "total"
          }
      }
  , E.BarSeries
      { common: E.universalSeriesDefault
          { name = Just "email marketing"
          , tooltip = Just $ E.Tooltip $ E.tooltipDefault {trigger = Just E.TriggerItem}
          }
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
          , stack = Just "total"
          }
      }
  , E.BarSeries
      { common: E.universalSeriesDefault
          { name = Just "affiliate advertising"
          , tooltip = Just $ E.Tooltip $ E.tooltipDefault {trigger = Just E.TriggerItem}
       }
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
          }
      }
  , E.BarSeries
      { common: E.universalSeriesDefault
           { name = Just "video ads"
           , tooltip = Just $ E.Tooltip $ E.tooltipDefault {trigger = Just E.TriggerItem}
           }
      , barSeries: E.barSeriesDefault
          { "data" = Just $ map simpleData [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
          }
      }
  , E.LineSeries
     { common: E.universalSeriesDefault
         { name = Just "must be"
         }
     , lineSeries: E.lineSeriesDefault
         { "data" = Just $ map simpleData [862.0, 1018.0, 964.0, 1026.0, 1679.0, 1600.0, 1570.0]
         }
     }
  , E.PieSeries
      { common: E.universalSeriesDefault
          { name = Just "search engine"
          , itemStyle = Just $ E.ItemStyle
              { emphasis: Nothing
              , normal: Just $ E.IStyle $ E.istyleDefault
                  { labelLine = Just $ E.ItemLabelLine
                      { show: Just true
                      , length: Just 20.0
                      , lineStyle: Nothing
                      }
                  }
              }
          , tooltip = Just $ E.Tooltip $ E.tooltipDefault
              { trigger = Just E.TriggerItem
              , formatter = Just $ E.Template "{a} <br/> {b}: {c} ({d}%)"
              }
          }
      , pieSeries: E.pieSeriesDefault
          { radius = Just $ E.Rs {inner: (E.Pixel 0.0), outer: (E.Pixel 50.0)}
          , center = Just $ Tuple (E.Percent 160.0) (E.Percent 130.0)
          , "data" = Just $ map E.Dat
              [ (E.dataDefault (E.Simple 1047.0))
                  { name = Just "Baidu"}
              , (E.dataDefault (E.Simple 264.0))
                  { name = Just "Google" }
              , (E.dataDefault (E.Simple 145.0))
                  { name = Just "Bing" }
              , (E.dataDefault (E.Simple 102.0))
                  { name = Just "other" }

              ]
          }
      }
  ]


options ∷ E.Option
options =
  E.Option $ E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerAxis}
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { show = Just true
        , y = Just E.YBottom
        , feature = Just $ E.Feature
            { mark: Just $ E.MarkFeature
                { show: Just true
                , title: Nothing
                , lineStyle: Nothing
                }
            , dataView: Just $ E.DataViewFeature
                { show: Just true
                , title: Nothing
                , readOnly: Just true
                , lang: Nothing
                }
            , magicType: Just $ E.MagicTypeFeature
                { show: Just true
                , title: Nothing
                , option: Nothing
                , "type": Just [E.MagicLine, E.MagicBar, E.MagicStack, E.MagicTiled]
                }
            , restore: Just $ E.RestoreFeature
                { show: Just true
                , title: Nothing
                }
            , saveAsImage: Just $ E.SaveAsImageFeature
                { show: Just true
                , title: Nothing
                , "type": Nothing
                , lang: Nothing
                }
            , dataZoom: Nothing
            }
        }
    , calculable = Just true
    , legend = Just $ E.Legend $ E.legendDefault
        { "data" = Just $ map E.legendItemDefault
            [ "direct access"
            , "email marketing"
            , "affiliate advertising"
            , "video ads"
            , "search engine"
            , "Baidu"
            , "Google"
            , "must be"
            , "other"
            ]
    }
    , xAxis = Just $ E.OneAxis $ E.Axis $ E.axisDefault
        { "type" = Just E.CategoryAxis
        , splitLine = Just $ E.AxisSplitLine $ E.axisSplitLineDefault
            { show = Just false }
        , "data" = Just $ map E.CommonAxisData
            [ "Monday"
            , "Tuesday"
            , "Wednesday"
            , "Thursday"
            , "Friday"
            , "Saturday"
            , "Sunday"
            ]
        }
    , yAxis = Just $ E.OneAxis $ E.Axis $ E.axisDefault
        { "type" = Just E.ValueAxis
        , position = Just E.RightAxis
        }
    , series = Just $ map Just series
   }

mix2safe ∷ ∀ e. ElementId → Eff (dom ∷ DOM, console ∷ CONSOLE, echarts ∷ E.ECHARTS |e) Unit
mix2safe id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in mix2safe"
    Just el →
      E.init Nothing el
        >>= E.setOption options true
        >>= \_ → pure unit
