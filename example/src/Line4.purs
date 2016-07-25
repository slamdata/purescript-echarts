module Line4 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U

simpleData ∷ Number → E.ItemData
simpleData = E.Value <<< E.Simple

options ∷ E.Option
options =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerAxis}
    , legend = Just $ E.Legend E.legendDefault
        { x = Just E.XLeft
        , "data" = Just $ map E.legendItemDefault
            [ "email marketing"
            , "affiliate advertising"
            , "video ads"
            , "direct access"
            , "search engine"
            ]
        }
    , toolbox = Just $ E.Toolbox $ E.toolboxDefault
        { show = Just true
        , x = Just E.XRight
        , y = Just E.YBottom
        , feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature E.markFeatureDefault {show = Just true}
            , dataView = Just $ E.DataViewFeature E.dataViewFeatureDefault
                { show = Just true
                , readOnly = Just false
                }
            , magicType = Just $ E.MagicTypeFeature E.magicTypeFeatureDefault
                { show = Just true
                , "type" = Just [E.MagicLine, E.MagicBar, E.MagicStack, E.MagicTiled]
                }
            , restore = Just $ E.RestoreFeature E.restoreFeatureDefault
                { show = Just true }
            , saveAsImage = Just $ E.SaveAsImageFeature E.saveAsImageFeatureDefault
                { show = Just true }

            }
        }
    , calculable = Just true
    , xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.CategoryAxis
        , boundaryGap = Just $ E.CatBoundaryGap false
        , "data" = Just $ map E.CommonAxisData
            ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
        }
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.ValueAxis }
    , series = Just $ map Just
        [ E.LineSeries
            { common: E.universalSeriesDefault { name = Just "email marketing" }
            , lineSeries: E.lineSeriesDefault
                { stack = Just "total"
                , "data" = Just $ map simpleData [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
                }
            }
        , E.LineSeries
            { common: E.universalSeriesDefault { name = Just "affiliate advertising" }
            , lineSeries: E.lineSeriesDefault
                { "data" = Just $ map simpleData [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0] }
            }
        , E.LineSeries
          { common: E.universalSeriesDefault { name = Just "video ads" }
          , lineSeries: E.lineSeriesDefault
              { stack = Just "total"
              , "data" = Just $ map simpleData [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
              }
          }
        , E.LineSeries
            { common: E.universalSeriesDefault { name = Just "direct access" }
            , lineSeries: E.lineSeriesDefault
                { "data" = Just $ map simpleData [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0] }
            }
        , E.LineSeries
            { common: E.universalSeriesDefault { name = Just "search engine" }
            , lineSeries: E.lineSeriesDefault
                { stack = Just "total"
                , "data" = Just $ map simpleData [820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0]
                }
            }
        ]
    }

line4
  ∷ ∀ e
  . ElementId
  → Eff (console ∷ CONSOLE, dom ∷ DOM, echarts ∷ E.ECHARTS |e) Unit
line4 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in line4"
    Just el → do
      chart ← E.init Nothing el
      chart' ← E.setOption options true chart
      pure unit
