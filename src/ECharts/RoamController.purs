module ECharts.RoamController where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators

import Data.Maybe
import Data.StrMap

import ECharts.Coords
import ECharts.Color
import ECharts.Common

type RoamControllerRec = {
    show :: Maybe Boolean,
    x :: Maybe XPos,
    y :: Maybe YPos,
    width :: Maybe Number,
    height :: Maybe Number,
    backgroundColor :: Maybe Color,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number,
    padding :: Maybe (Corner Number),
    fillerColor :: Maybe Color,
    handleColor :: Maybe Color,
    step :: Maybe Number,
    mapTypeControl :: Maybe (StrMap Boolean)
    }

newtype RoamController = RoamController RoamControllerRec
   
instance roamControllerEncodeJson :: EncodeJson RoamController where
  encodeJson (RoamController obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "x" := obj.x,
      "y" := obj.y,
      "width" := obj.width,
      "height" := obj.height,
      "backgroundColor" := obj.backgroundColor,
      "borderColor" := obj.borderColor,
      "borderWidth" := obj.borderWidth,
      "padding" := obj.padding,
      "fillerColor" := obj.fillerColor,
      "handleColor" := obj.handleColor,
      "step" := obj.step,
      "mapTypeControl" := obj.mapTypeControl
    ]
roamControllerDefault :: RoamControllerRec
roamControllerDefault = {
  show: Nothing,
  x: Nothing,
  y: Nothing,
  width: Nothing,
  height: Nothing,
  backgroundColor: Nothing,
  borderColor: Nothing,
  borderWidth: Nothing,
  padding: Nothing,
  fillerColor: Nothing,
  handleColor: Nothing,
  step: Nothing,
  mapTypeControl: Nothing
  }
