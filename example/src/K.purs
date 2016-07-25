module K where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import ECharts as E

import Utils as U


simpleDat ∷ Number → Number → Number → Number → E.ItemData
simpleDat o c l h = E.Value $ E.HLOC {o, c, l, h }

options ∷ E.Option
options =
  E.Option E.optionDefault
    { xAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.CategoryAxis
        , "data" = Just $ map E.CommonAxisData
            ["2013/1/24", "2013/1/25", "2013/1/28", "2013/1/29", "2013/1/30"]
        }
    , yAxis = Just $ E.OneAxis $ E.Axis E.axisDefault
        { "type" = Just E.ValueAxis
        , min = Just 2200.0,
          scale = Just true
        }
    , series = Just $ map Just
        [ E.CandlestickSeries
            { common: E.universalSeriesDefault
            , candlestickSeries: E.candlestickSeriesDefault
                { "data" = Just
                    [ simpleDat 2320.26 2302.6 2287.3 2362.94
                    , simpleDat 2300.0 2291.3 2288.26 2308.38
                    , simpleDat 2295.35 2346.5 2295.35 2346.92
                    , simpleDat 2347.22 2358.98 2337.35 2363.8
                    , simpleDat 2360.75 2382.48 2347.89 2383.76
                    ]
                }
            }
        ]
    }


k ∷ ∀ e. ElementId → Eff (console ∷ CONSOLE, echarts ∷ E.ECHARTS, dom ∷ DOM|e) Unit
k id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in k"
    Just el → E.init Nothing el >>= E.setOption options true >>= \_ -> pure unit
