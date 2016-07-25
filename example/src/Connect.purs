module Connect where

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

options1 ∷ E.Option
options1 =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault
        { trigger = Just E.TriggerItem
        , formatter = Just $ E.Template "{a} <br/> {b}: {c} ({d}%)"
        }
    , title = Just $ E.Title E.titleDefault
        { text = Just "a site user to access source"
        , subtext = Just "fictitious"
        , x = Just E.XRight
        }
    , legend = Just $ E.Legend E.legendDefault
        { orient = Just E.Vertical
        , x = Just E.XLeft
        , "data" = Just $ map E.legendItemDefault
            [ "direct access"
            , "email marketing"
            , "affiliate advertising"
            , "video ads"
            , "search engine"
            ]
        }
    , calculable = Just true
    , series = Just $ map Just
        [ E.PieSeries
            { common: E.universalSeriesDefault { name = Just "access to the source." }
            , pieSeries: E.pieSeriesDefault
                { radius = Just $ E.R (E.Percent 55.0)
                , center = Just $ Tuple (E.Percent 50.0) (E.Pixel 225.0),
                  "data" = Just
                    [ E.Dat $ (E.dataDefault $ E.Simple 335.0) {name = Just "direct access"}
                    , E.Dat $ (E.dataDefault $ E.Simple 310.0) {name = Just "email marketing"}
                    , E.Dat $ (E.dataDefault $ E.Simple 234.0) {name = Just "affiliate advertising"}
                    , E.Dat $ (E.dataDefault $ E.Simple 135.0) {name = Just "video ads"}
                    , E.Dat $ (E.dataDefault $ E.Simple 158.0) {name = Just "search engine"}
                    ]
                }
            }
        ]
    }

options2 ∷ E.Option
options2 =
  E.Option E.optionDefault
    { tooltip = Just $ E.Tooltip E.tooltipDefault
        { trigger = Just E.TriggerAxis
        , axisPointer = Just $ E.TooltipAxisPointer E.tooltipAxisPointerDefault
            { "type" = Just E.ShadowPointer }
        }
    , legend = Just $ E.Legend E.legendDefault
        { "data" = Just $ map E.legendItemDefault
            [ "direct access"
            , "email marketing"
            , "affiliate advertising"
            , "video ads"
            , "search engine"
            ]
        }
    , toolbox = Just $ E.Toolbox E.toolboxDefault
        { feature = Just $ E.Feature E.featureDefault
            { mark = Just $ E.MarkFeature E.markFeatureDefault {show = Just true}
            , magicType = Just $ E.MagicTypeFeature E.magicTypeFeatureDefault
                { show = Just true
                , "type" = Just [E.MagicLine, E.MagicBar, E.MagicStack, E.MagicTiled]
                }
            , restore = Just $ E.RestoreFeature E.restoreFeatureDefault { show = Just true }
            , saveAsImage = Just $ E.SaveAsImageFeature E.saveAsImageFeatureDefault
                { show = Just true }
            }
        , show = Just true
        , y = Just E.YBottom
        }
    , calculable = Just true
    , xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.CategoryAxis
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
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.ValueAxis
        , splitArea = Just $ E.AxisSplitArea E.axisSplitAreaDefault { show = Just true }
        }
    , grid = Just $ E.Grid E.gridDefault {x2 = Just $ E.Pixel 40.0}
    , series = Just $ map Just
        [ E.BarSeries
          { common: E.universalSeriesDefault { name = Just "direct access" }
          , barSeries: E.barSeriesDefault
              { stack = Just "total"
              , "data" = Just $ map simpleData [320.0, 332.0, 301.0, 334.0, 390.0, 330.0, 320.0]
              }
          }
        , E.BarSeries
            { common: E.universalSeriesDefault { name = Just "email marketing" }
            , barSeries: E.barSeriesDefault
                { stack = Just "email marketing"
                , "data" = Just $ map simpleData [120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
                }
            }
        , E.BarSeries
            { common: E.universalSeriesDefault { name = Just "affiliate advertising" }
            , barSeries: E.barSeriesDefault
                { stack = Just "total"
                , "data" = Just $ map simpleData [220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
                }
            }
        , E.BarSeries
            { common: E.universalSeriesDefault { name = Just "video Ads" }
            , barSeries: E.barSeriesDefault
                { stack = Just "total"
                , "data" = Just $ map simpleData [150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
                }
            }
        , E.BarSeries
            { common: E.universalSeriesDefault { name = Just "search engine" }
            , barSeries: E.barSeriesDefault
                { stack = Just "total"
                , "data" = Just $ map simpleData [820.0, 932.0, 901.0, 934.0, 1290.0, 1330.0, 1320.0]
                }
            }
        ]
    }

conn ∷ ∀ e. E.EChart → E.EChart → Eff (echarts ∷ E.ECHARTS|e) Unit
conn first second = do
  E.connect first second
  E.connect second first
  pure unit

connectM
  ∷ ∀ e
  . ElementId
  → ElementId
  → Eff (dom ∷ DOM, echarts ∷ E.ECHARTS, console ∷ CONSOLE|e) Unit
connectM firstId secondId = do
  mbElFst ← U.getElementById firstId
  mbElSnd ← U.getElementById secondId

  case Tuple mbElFst mbElSnd  of
    Tuple Nothing _ → log "incorrect first id in connect"
    Tuple _ Nothing → log "incorrect second id in connect"
    Tuple (Just first) (Just second) → do
      fst ← E.init Nothing first >>= E.setOption options1 true
      snd ← E.init Nothing second >>= E.setOption options2 true
      conn fst snd
      pure unit
