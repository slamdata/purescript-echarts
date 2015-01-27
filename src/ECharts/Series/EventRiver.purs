module ECharts.Series.EventRiver where

import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap
import Data.Date (Date(..))
import Data.Argonaut.Extension.Date

import ECharts.Common
import ECharts.Coords
import ECharts.Chart
import ECharts.Tooltip
import ECharts.Type
import ECharts.Style.Item
import ECharts.Mark.Line
import ECharts.Mark.Point
import ECharts.Item.Data
import ECharts.Symbol
import ECharts.Series.Force
import ECharts.Series.Gauge
import ECharts.Axis
import ECharts.Title

newtype EvolutionDetail =
  EvolutionDetail {
    "link" :: Maybe String,
    "text" :: Maybe String,
    "img" :: Maybe String
    }

instance evoDetailEncodeJson :: EncodeJson EvolutionDetail where
  encodeJson (EvolutionDetail e) = fromObject $ fromList $ [
    "link" := e.link,
    "text" := e.text,
    "img" := e.img
    ]

evolutionDetailDefault = {
  link: Nothing,
  text: Nothing,
  img: Nothing
  }

newtype Evolution =
  Evolution {
    "time" :: Date,
    "value" :: Number,
    "detail" :: Maybe EvolutionDetail
    }

instance evoEncodeJson :: EncodeJson Evolution where
  encodeJson (Evolution e) = fromObject $ fromList $ [
    "time" := e.time,
    "value" := e.value,
    "detail" := e.detail
    ]

newtype OneEvent =
  OneEvent {
    "name" :: Maybe String,
    "weight" :: Maybe Number,
    "evolution" :: Maybe [Evolution]
    }

oneEventDefault = {
  name: Nothing,
  weight: Nothing,
  evolution: Nothing
  }

instance oneEventEncodeJson :: EncodeJson OneEvent where
  encodeJson (OneEvent oe) = fromObject $ fromList $ [
    "name" := oe.name,
    "weight" := oe.weight,
    "evolution" := oe.evolution
    ]
