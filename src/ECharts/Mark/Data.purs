module ECharts.Mark.Data where

import ECharts.Prelude

import Data.StrMap as SM

type MarkPointDataRec =
  { name ∷ Maybe String
  , value ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , xAxis ∷ Maybe Number
  , yAxis ∷ Maybe Number
  , "type" ∷ Maybe String
  }

newtype MarkPointData
  = MarkPointData MarkPointDataRec

instance mpDataEncodeJson ∷ EncodeJson MarkPointData where
  encodeJson (MarkPointData mp) =
    encodeJson
      $ SM.fromFoldable
        [ "name" := mp.name
        , "value" := mp.value
        , "x" := mp.x
        , "y" := mp.y
        , "xAxis" := mp.xAxis
        , "yAxis" := mp.yAxis
        , "type" := mp."type"
        ]

instance mpDataDecodeJson ∷ DecodeJson MarkPointData where
  decodeJson j = do
    o ← decodeJson j
    r ← { name: _
        , value: _
        , x: _
        , y: _
        , xAxis: _
        , yAxis: _
        , "type": _ }
        <$> (o .? "name")
        <*> (o .? "value")
        <*> (o .? "x")
        <*> (o .? "y")
        <*> (o .? "xAxis")
        <*> (o .? "yAxis")
        <*> (o .? "type")
    pure $ MarkPointData r

markPointDataDefault ∷ MarkPointDataRec
markPointDataDefault =
  { name: Nothing
  , value: Nothing
  , x: Nothing
  , y: Nothing
  , xAxis: Nothing
  , yAxis: Nothing
  , "type": Nothing
  }
