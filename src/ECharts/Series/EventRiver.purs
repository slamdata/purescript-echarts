module ECharts.Series.EventRiver
  ( EvolutionDetail(..)
  , EvolutionDetailRec
  , evolutionDetailDefault
  , Evolution(..)
  , EvolutionRec
  , OneEvent(..)
  , OneEventRec
  , oneEventDefault
  ) where

import ECharts.Prelude

import Data.StrMap as SM
import Data.DateTime as DT
import Data.JSDate as JSD

type EvolutionDetailRec =
  { link ∷ Maybe String
  , text ∷ Maybe String
  , img ∷ Maybe String
  }

newtype EvolutionDetail
  = EvolutionDetail EvolutionDetailRec


instance evoDetailEncodeJson ∷ EncodeJson EvolutionDetail where
  encodeJson (EvolutionDetail e) =
    encodeJson
      $ SM.fromFoldable
          [ "link" := e.link
          , "text" := e.text
          , "img" := e.img
          ]

instance evoDetailDecodeJson ∷ DecodeJson EvolutionDetail where
  decodeJson j = do
    o ← decodeJson j
    r ← { link: _
        , text: _
        , img: _ }
        <$> (o .? "link")
        <*> (o .? "text")
        <*> (o .? "img")
    pure $ EvolutionDetail r


evolutionDetailDefault ∷ EvolutionDetailRec
evolutionDetailDefault =
  { link: Nothing
  , text: Nothing
  , img: Nothing
  }

type EvolutionRec =
  { time ∷ DT.DateTime
  , value ∷ Number
  , detail ∷ Maybe EvolutionDetail
  }

newtype Evolution
  = Evolution EvolutionRec

foreign import jsDateToJson ∷ JSD.JSDate → Json

foreign import jsonToJSDate ∷ Json → JSD.JSDate

dateToJson ∷ DT.DateTime → Json
dateToJson = jsDateToJson <<< JSD.fromDateTime

instance evoEncodeJson ∷ EncodeJson Evolution where
  encodeJson (Evolution e) =
    encodeJson
      $ SM.fromFoldable
          [ "time" := dateToJson e.time
          , "value" := e.value
          , "detail" := e.detail
          ]

instance evoDecodeJson ∷ DecodeJson Evolution where
  decodeJson j = do
    o ← decodeJson j
    t ← o .? "time"
    time ←
      jsonToJSDate t
      # JSD.toDateTime
      # maybe (Left "incorrect datetime") pure
    r ← { time: time
        , value: _
        , detail: _ }
        <$> (o .? "value")
        <*> (o .? "detail")
    pure $ Evolution r

type OneEventRec =
  { name ∷ Maybe String
  , weight ∷ Maybe Number
  , evolution ∷ Maybe (Array Evolution)
  }

newtype OneEvent
  = OneEvent OneEventRec

oneEventDefault ∷ OneEventRec
oneEventDefault =
  { name: Nothing
  , weight: Nothing
  , evolution: Nothing
  }

instance oneEventEncodeJson ∷ EncodeJson OneEvent where
  encodeJson (OneEvent oe) =
    encodeJson
      $ SM.fromFoldable
          [ "name" := oe.name
          , "weight" := oe.weight
          , "evolution" := oe.evolution
          ]

instance oneEventDecodeJson ∷ DecodeJson OneEvent where
  decodeJson j = do
    o ← decodeJson j
    r ← { name: _
        , weight: _
        , evolution: _ }
        <$> (o .? "name")
        <*> (o .? "weight")
        <*> (o .? "evolution")
    pure $ OneEvent r
