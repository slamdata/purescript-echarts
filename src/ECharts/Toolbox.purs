module ECharts.Toolbox where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Argonaut.Combinators
import Data.Maybe
import Data.StrMap
import ECharts.Color
import ECharts.Coords
import ECharts.Common
import ECharts.Style.Text
import ECharts.Style.Line
import ECharts.Image
import ECharts.Type

newtype Toolbox =
  Toolbox {
    "show" :: Maybe Boolean,
    "orient" :: Maybe Orient,
    "x" :: Maybe XPos,
    "y" :: Maybe YPos,
    "backgroundColor" :: Maybe Color,
    "borderColor" :: Maybe Color,
    "borderWidth" :: Maybe Number,
    "padding" :: Maybe (Corner Number),
    "itemGap" :: Maybe Number,
    "itemSize" :: Maybe Number,
    "color" :: Maybe [Color],
    "disableColor" :: Maybe Color,
    "effectiveColor" :: Maybe Color,
    "showTitle" :: Maybe Boolean,
    "textStyle" :: Maybe TextStyle,
    "feature" :: Maybe Feature
    }

toolboxDefault =
  {
    "show": Nothing,
    "orient": Nothing,
    "x": Nothing,
    "y": Nothing,
    "backgroundColor": Nothing,
    "borderColor": Nothing,
    "borderWidth": Nothing,
    "padding": Nothing,
    "itemGap": Nothing,
    "itemSize": Nothing,
    "color": Nothing,
    "disableColor": Nothing,
    "effectiveColor": Nothing,
    "showTitle": Nothing,
    "textStyle": Nothing,
    "feature": Nothing
  }
  

instance toolboxEncodeJson :: EncodeJson Toolbox where
  encodeJson (Toolbox obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "orient" := obj.orient,
      "x" := obj.x,
      "y" := obj.y,
      "backgroundColor" := obj.backgroundColor,
      "borderColor" := obj.borderColor,
      "borderWidth" := obj.borderWidth,
      "padding" := obj.padding,
      "itemGap" := obj.itemGap,
      "itemSize" := obj.itemSize,
      "color" := obj.color,
      "disableColor" := obj.disableColor,
      "effectiveColor" := obj.effectiveColor,
      "showTitle" := obj.showTitle,
      "textStyle" := obj.textStyle,
      "feature" := obj.feature
    ]


newtype Feature =
  Feature {
    "mark" :: Maybe MarkFeature,
    "dataZoom" :: Maybe DataZoomFeature,
    "dataView" :: Maybe DataViewFeature,
    "magicType" :: Maybe MagicTypeFeature,
    "restore" :: Maybe RestoreFeature,
    "saveAsImage" :: Maybe SaveAsImageFeature
    }


instance featureEncodeJson :: EncodeJson Feature where
  encodeJson (Feature obj) =
    fromObject $ fromList $
    [
      "mark" := obj.mark,
      "dataZoom" := obj.dataZoom,
      "dataView" := obj.dataView,
      "magicType" := obj.magicType,
      "restore" := obj.restore,
      "saveAsImage" := obj.saveAsImage
    ]

featureDefault = {
  mark: Nothing,
  dataZoom: Nothing,
  dataView: Nothing,
  magicType: Nothing,
  restore: Nothing,
  saveAsImage: Nothing
  }

newtype SaveAsImageFeature =
  SaveAsImageFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe String,
    "type" :: Maybe ImgType,
    "lang" :: Maybe [String]
    }

instance saveAsImageEncodeJson :: EncodeJson SaveAsImageFeature where
  encodeJson (SaveAsImageFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title,
      "type" := obj.type,
      "lang" := obj.lang
    ]
saveAsImageFeatureDefault = {
  show: Nothing,
  title: Nothing,
  type: Nothing,
  lang: Nothing
  }

newtype RestoreFeature =
  RestoreFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe String
    }

instance restoreFeatureEncodeJson :: EncodeJson RestoreFeature where
  encodeJson (RestoreFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title
    ]

restoreFeatureDefault = {
  show: Nothing,
  title: Nothing
  }

newtype DataZoomFeatureTitle =
  DataZoomFeatureTitle {
    dataZoom :: String,
    dataZoomReset :: String
    }
instance datazoomTitleEncodeJson :: EncodeJson DataZoomFeatureTitle where
  encodeJson (DataZoomFeatureTitle obj) =
    fromObject $ fromList $
    [
      "dataZoom" := obj.dataZoom,
      "dataZoomReset" := obj.dataZoomReset
    ]
  
newtype DataZoomFeature =
  DataZoomFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe DataZoomFeatureTitle
    }

dataZoomFeatureDefault = {
  show: Nothing,
  title: Nothing
  }
  
instance dataviewFeatureEncodeJson :: EncodeJson DataZoomFeature where
  encodeJson (DataZoomFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title
    ]


newtype DataViewFeature =
  DataViewFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe String,
    "readOnly" :: Maybe Boolean,
    "lang" :: Maybe [String]
    }

dataViewFeatureDefault = {
  show: Nothing,
  title: Nothing,
  readOnly: Nothing,
  lang: Nothing
  }

instance dataViewFeatureEncodeJson :: EncodeJson DataViewFeature where
  encodeJson (DataViewFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title,
      "readOnly" := obj.readOnly,
      "lang" := obj.lang
    ]


newtype MarkFeatureTitle =
  MarkFeatureTitle {
    "mark" :: Maybe String,
    "markUndo" :: String,
    "markClear" :: String
    }

instance mftitleEncodeJson :: EncodeJson MarkFeatureTitle where
  encodeJson (MarkFeatureTitle obj) =
    fromObject $ fromList $
    [
      "mark" := obj.mark,
      "markUndo" := obj.markUndo,
      "markClear" := obj.markUndo
    ]

newtype MarkFeature =
  MarkFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe MarkFeatureTitle,
    "lineStyle" :: Maybe LineStyle
    }

instance markFeatureEncodeJson :: EncodeJson MarkFeature where
  encodeJson (MarkFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title,
      "lineStyle" := obj.lineStyle
    ]

markFeatureDefault = {
  show: Nothing,
  title: Nothing,
  lineStyle: Nothing
  }

data MagicType = MagicLine | MagicBar | MagicStack | MagicTiled | MagicForce
               | MagicChord | MagicPie | MagicFunnel

instance magicTypeEncodeJson :: EncodeJson MagicType where
  encodeJson a = encodeJson $ case a of
    MagicLine -> "line"
    MagicBar -> "bar"
    MagicStack -> "stack"
    MagicTiled -> "tiled"
    MagicForce -> "force"
    MagicChord -> "chord"
    MagicPie -> "pie"
    MagicFunnel -> "funnel"

newtype MagicTypeFeature =
  MagicTypeFeature {
    "show" :: Maybe Boolean,
    "title" :: Maybe (StrMap String),
    "option" :: Maybe Json,
    "type" :: Maybe [MagicType]
    }

magicTypeFeature = {
  show: Nothing,
  title: Nothing,
  option: Nothing,
  type: Nothing
  }

instance magicTypeFeatureEncodeJson :: EncodeJson MagicTypeFeature where
  encodeJson (MagicTypeFeature obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "title" := obj.title,
      "option" := obj.option,
      "type" := obj.type
    ]
