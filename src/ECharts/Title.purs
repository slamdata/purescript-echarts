module ECharts.Title where

import Prelude
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Decode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.Either 
import Data.StrMap hiding (toList)
import Data.List (toList)

import ECharts.Color
import ECharts.Coords
import ECharts.Common
import ECharts.Style.Text

data LinkTarget = Self | Blank

instance linkTargetEncodeJson :: EncodeJson LinkTarget where
  encodeJson a = fromString $ case a of
    Self -> "self"
    Blank -> "blank"

instance linkTargetDecodeJson :: DecodeJson LinkTarget where
  decodeJson j = do
    str <- decodeJson j
    case str of
      "self" -> pure Self
      "blank" -> pure Blank
      _ -> Left "incorrect link target"

type TitleRec = {
    text :: Maybe String,
    link :: Maybe String,
    target :: Maybe LinkTarget,
    subtext :: Maybe String,
    sublink :: Maybe String,
    subtarget :: Maybe LinkTarget,
    x :: Maybe XPos,
    y :: Maybe YPos,
    textAlign :: Maybe HorizontalAlign,
    backgroundColor :: Maybe Color,
    borderColor :: Maybe Color,
    borderWidth :: Maybe Number,
    padding :: Maybe (Corner Number),
    itemGap :: Maybe Number,
    textStyle :: Maybe TextStyle,
    subtextStyle :: Maybe TextStyle
    }

newtype Title = Title TitleRec


instance titleEncodeJson :: EncodeJson Title where
  encodeJson (Title obj) =
    fromObject $ fromList $ toList $ 
    [
      "text" := obj.text,
      "link" := obj.link,
      "target" := obj.target,
      "subtext" := obj.subtext,
      "sublink" := obj.sublink,
      "subtarget" := obj.subtarget,
      "x" := obj.x,
      "y" := obj.y,
      "textAlign" := obj.textAlign,
      "backgroundColor" := obj.backgroundColor,
      "borderColor" := obj.borderColor,
      "borderWidth" := obj.borderWidth,
      "padding" := obj.padding,
      "itemGap" := obj.itemGap,
      "textStyle" := obj.textStyle,
      "subtextStyle" := obj.subtextStyle
    ]

instance titleDecodeJson :: DecodeJson Title where
  decodeJson j = do
    o <- decodeJson j
    r <- { text: _
         , link: _
         , target: _
         , subtext: _
         , sublink: _
         , subtarget: _
         , x: _
         , y: _
         , textAlign: _
         , backgroundColor: _
         , borderColor: _
         , borderWidth: _
         , padding: _
         , itemGap: _
         , textStyle: _
         , subtextStyle: _ } <$>
         (o .? "text") <*>
         (o .? "link") <*>
         (o .? "target") <*>
         (o .? "subtext") <*>
         (o .? "sublink") <*>
         (o .? "subtarget") <*>
         (o .? "x") <*>
         (o .? "y") <*>
         (o .? "textAlign") <*>
         (o .? "backgroundColor") <*>
         (o .? "borderColor") <*>
         (o .? "borderWidth") <*>
         (o .? "padding") <*>
         (o .? "itemGap") <*>
         (o .? "textStyle") <*>
         (o .? "subtextStyle")
    pure $ Title r
             


titleDefault :: TitleRec
titleDefault = {
  text: Nothing,
  link: Nothing,
  subtext: Nothing,
  sublink: Nothing,
  subtarget: Nothing,
  x: Nothing,
  y: Nothing,
  textAlign: Nothing,
  backgroundColor: Nothing,
  borderColor: Nothing,
  padding: Nothing,
  itemGap: Nothing,
  textStyle: Nothing,
  subtextStyle: Nothing,
  borderWidth: Nothing,
  target: Nothing
  }
