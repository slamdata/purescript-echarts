module ECharts.Series.EventRiver (
  EvolutionDetail(..),
  EvolutionDetailRec(),
  evolutionDetailDefault,
  Evolution(..),
  EvolutionRec(),
  OneEvent(..),
  OneEventRec(),
  oneEventDefault
  ) where 

import Data.Maybe
import Data.Either
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Tuple
import Data.StrMap
import Data.Date (Date(..), JSDate(), toJSDate, fromStringStrict)


import ECharts.Common
import ECharts.Coords
import ECharts.Chart
import ECharts.Tooltip
import ECharts.Style.Item
import ECharts.Mark.Line
import ECharts.Mark.Point
import ECharts.Item.Data
import ECharts.Symbol
import ECharts.Series.Force
import ECharts.Series.Gauge
import ECharts.Axis
import ECharts.Title

type EvolutionDetailRec = {
    link :: Maybe String,
    text :: Maybe String,
    img :: Maybe String
    }

newtype EvolutionDetail = EvolutionDetail EvolutionDetailRec
   

instance evoDetailEncodeJson :: EncodeJson EvolutionDetail where
  encodeJson (EvolutionDetail e) = fromObject $ fromList $ [
    "link" := e.link,
    "text" := e.text,
    "img" := e.img
    ]

instance evoDetailDecodeJson :: DecodeJson EvolutionDetail where
  decodeJson j = do
    o <- decodeJson j
    r <- { link: _
         , text: _
         , img: _ } <$>
         (o .? "link") <*>
         (o .? "text") <*>
         (o .? "img")
    pure $ EvolutionDetail r 

                                   
evolutionDetailDefault :: EvolutionDetailRec
evolutionDetailDefault = {
  link: Nothing,
  text: Nothing,
  img: Nothing
  }

type EvolutionRec = {
    time :: Date,
    value :: Number,
    detail :: Maybe EvolutionDetail
    }

newtype Evolution = Evolution EvolutionRec

foreign import jsDateToJson """
function jsDateToJson(date) {
  return date;
}
""" :: JSDate -> Json 

dateToJson :: Date -> Json
dateToJson = jsDateToJson <<< toJSDate

instance evoEncodeJson :: EncodeJson Evolution where
  encodeJson (Evolution e) = fromObject $ fromList $ [
    "time" := dateToJson e.time,
    "value" := e.value,
    "detail" := e.detail
    ]

instance evoDecodeJson :: DecodeJson Evolution where
  decodeJson j = do
    o <- decodeJson j
    t <- o .? "time"
    time <- maybe (Left "incorrect time") Right $ fromStringStrict t 
    r <- { time: time
         , value: _
         , detail: _ } <$>
         (o .? "value") <*>
         (o .? "detail")
    pure $ Evolution r

type OneEventRec = {
    name :: Maybe String,
    weight :: Maybe Number,
    evolution :: Maybe [Evolution]
    }

newtype OneEvent = OneEvent OneEventRec
   
oneEventDefault :: OneEventRec
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

instance oneEventDecodeJson :: DecodeJson OneEvent where
  decodeJson j = do
    o <- decodeJson j
    r <- { name: _
         , weight: _
         , evolution: _ } <$>
         (o .? "name") <*>
         (o .? "weight") <*>
         (o .? "evolution")
    pure $ OneEvent r
