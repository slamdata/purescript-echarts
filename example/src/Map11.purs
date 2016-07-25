module Map11 where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.Node.Types (ElementId)

import Utils as U

import ECharts as E

nameValue ∷ ∀ r. {name ∷ String, value ∷ Number|r} → E.MarkPointData
nameValue {name, value} =
  E.MarkPointData $ E.markPointDataDefault { name = Just name, value = Just value }

option ∷ E.Option
option =
  E.Option $ E.optionDefault
  { tooltip = Just $ E.Tooltip E.tooltipDefault {trigger = Just E.TriggerItem}
  , series = Just $ map Just
      [ E.MapSeries
          { common: E.universalSeriesDefault
              { markPoint = Just $ E.MarkPoint E.markPointDefault
                  { "data" = Just $ map nameValue [ {name: "trololo", value: 123.0} ] }
              }
          , mapSeries: E.mapSeriesDefault
              { "data" = Just []
              , mapType = Just "china"
              , geoCoord = Just $ fromFoldable
                  [ Tuple "trololo" (Tuple 121.0 43.0) ]
              }
          }
      ]
  }

map11 ∷ ∀ e. ElementId → Eff (console ∷ CONSOLE, echarts ∷ E.ECHARTS, dom ∷ DOM|e) Unit
map11 id = do
  mbEl ← U.getElementById id
  case mbEl of
    Nothing → log "incorrect id in map11"
    Just el → E.init Nothing el >>= E.setOption option true >>= \_ → pure unit
