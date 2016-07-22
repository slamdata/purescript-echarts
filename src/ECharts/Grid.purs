module ECharts.Grid where

import Prelude

import Data.Maybe
import Data.StrMap (fromList)
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.List (toList)

import ECharts.Common
import ECharts.Color

type GridRec =
  { x ∷ Maybe PercentOrPixel
  , x2 ∷ Maybe PercentOrPixel
  , y ∷ Maybe PercentOrPixel
  , y2 ∷ Maybe PercentOrPixel
  , width ∷ Maybe PercentOrPixel
  , height ∷ Maybe PercentOrPixel
  , backgroundColor ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , borderColor ∷ Maybe Number
  }

newtype Grid = Grid GridRec


instance gridEncodeJson ∷ EncodeJson Grid where
  encodeJson (Grid obj) =
    fromObject
      $ fromList
      $ toList
        [ "x" := obj.x
        , "y" := obj.y
        , "x2" := obj.x2
        , "y2" := obj.y2
        , "width" := obj.width
        , "height" := obj.height
        , "backgroundColor" := obj.backgroundColor
        , "borderWidth" := obj.borderWidth
        , "borderColor" := obj.borderColor
        ]

instance gridDecodeJson ∷ DecodeJson Grid where
  decodeJson j = do
    o ← decodeJson j
    r ← { x: _
        , y: _
        , x2: _
        , y2: _
        , width: _
        , height: _
        , backgroundColor: _
        , borderWidth: _
        , borderColor: _ }
        <$> (o .? "x")
        <*> (o .? "y")
        <*> (o .? "x2")
        <*> (o .? "y2")
        <*> (o .? "width")
        <*> (o .? "height")
        <*> (o .? "backgroundColor")
        <*> (o .? "borderWidth")
        <*> (o .? "borderColor")
    pure $ Grid r

gridDefault ∷ GridRec
gridDefault =
  { x: Nothing
  , y: Nothing
  , x2: Nothing
  , y2: Nothing
  , width: Nothing
  , height: Nothing
  , backgroundColor: Nothing
  , borderWidth: Nothing
  , borderColor: Nothing
  }
