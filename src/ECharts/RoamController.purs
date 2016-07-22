module ECharts.RoamController where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Coords (XPos, YPos)
import ECharts.Color (Color)
import ECharts.Common (Corner)

type RoamControllerRec =
  { show ∷ Maybe Boolean
  , x ∷ Maybe XPos
  , y ∷ Maybe YPos
  , width ∷ Maybe Number
  , height ∷ Maybe Number
  , backgroundColor ∷ Maybe Color
  , borderColor ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , padding ∷ Maybe (Corner Number)
  , fillerColor ∷ Maybe Color
  , handleColor ∷ Maybe Color
  , step ∷ Maybe Number
  , mapTypeControl ∷ Maybe (SM.StrMap Boolean)
  }

newtype RoamController
  = RoamController RoamControllerRec

instance roamControllerEncodeJson ∷ EncodeJson RoamController where
  encodeJson (RoamController obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "x" := obj.x
        , "y" := obj.y
        , "width" := obj.width
        , "height" := obj.height
        , "backgroundColor" := obj.backgroundColor
        , "borderColor" := obj.borderColor
        , "borderWidth" := obj.borderWidth
        , "padding" := obj.padding
        , "fillerColor" := obj.fillerColor
        , "handleColor" := obj.handleColor
        , "step" := obj.step
        , "mapTypeControl" := obj.mapTypeControl
        ]

instance roamControllerDecodeJson ∷ DecodeJson RoamController where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , x: _
        , y: _
        , width: _
        , height: _
        , backgroundColor: _
        , borderColor: _
        , borderWidth: _
        , padding: _
        , fillerColor: _
        , handleColor: _
        , step: _
        , mapTypeControl: _ }
        <$> (o .? "show")
        <*> (o .? "x")
        <*> (o .? "y")
        <*> (o .? "width")
        <*> (o .? "height")
        <*> (o .? "backgroundColor")
        <*> (o .? "borderColor")
        <*> (o .? "borderWidth")
        <*> (o .? "padding")
        <*> (o .? "fillerColor")
        <*> (o .? "handleColor")
        <*> (o .? "step")
        <*> (o .? "mapTypeControl")
    pure $ RoamController r

roamControllerDefault ∷ RoamControllerRec
roamControllerDefault =
  { show: Nothing
  , x: Nothing
  , y: Nothing
  , width: Nothing
  , height: Nothing
  , backgroundColor: Nothing
  , borderColor: Nothing
  , borderWidth: Nothing
  , padding: Nothing
  , fillerColor: Nothing
  , handleColor: Nothing
  , step: Nothing
  , mapTypeControl: Nothing
  }
