module ECharts.Title where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.StrMap

import ECharts.Color
import ECharts.Coords
import ECharts.Common
import ECharts.Style.Text

data LinkTarget = Self | Blank

instance linkTargetEncodeJson :: EncodeJson LinkTarget where
  encodeJson a = encodeJson $ case a of
    Self -> "self"
    Blank -> "blank"

newtype Title =
  Title {
    "text" :: Maybe String,
    "link" :: Maybe String,
    "target" :: Maybe LinkTarget,
    "subtext" :: Maybe String,
    "sublink" :: Maybe String,
    "subtarget" :: Maybe LinkTarget,
    "x" :: Maybe XPos,
    "y" :: Maybe YPos,
    "textAlign" :: Maybe HorizontalAlign,
    "backgroundColor" :: Maybe Color,
    "borderColor" :: Maybe Color,
    "borderWidth" :: Maybe Number,
    "padding" :: Maybe (Corner Number),
    "itemGap" :: Maybe Number,
    "textStyle" :: Maybe TextStyle,
    "subtextStyle" :: Maybe TextStyle
    }

instance titleEncodeJson :: EncodeJson Title where
  encodeJson (Title obj) =
    fromObject $ fromList $
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
