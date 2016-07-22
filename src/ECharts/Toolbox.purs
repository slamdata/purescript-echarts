module ECharts.Toolbox where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (Color)
import ECharts.Coords (XPos, YPos, Orient)
import ECharts.Common (Corner)
import ECharts.Style.Text (TextStyle)
import ECharts.Style.Line (LineStyle)
import ECharts.Image (ImgType)

type ToolboxRec =
  { show ∷ Maybe Boolean
  , orient ∷ Maybe Orient
  , x ∷ Maybe XPos
  , y ∷ Maybe YPos
  , backgroundColor ∷ Maybe Color
  , borderColor ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , padding ∷ Maybe (Corner Number)
  , itemGap ∷ Maybe Number
  , itemSize ∷ Maybe Number
  , color ∷ Maybe (Array Color)
  , disableColor ∷ Maybe Color
  , effectiveColor ∷ Maybe Color
  , showTitle ∷ Maybe Boolean
  , textStyle ∷ Maybe TextStyle
  , feature ∷ Maybe Feature
  }

newtype Toolbox
  = Toolbox ToolboxRec

toolboxDefault ∷ ToolboxRec
toolboxDefault =
  { show: Nothing
  , orient: Nothing
  , x: Nothing
  , y: Nothing
  , backgroundColor: Nothing
  , borderColor: Nothing
  , borderWidth: Nothing
  , padding: Nothing
  , itemGap: Nothing
  , itemSize: Nothing
  , color: Nothing
  , disableColor: Nothing
  , effectiveColor: Nothing
  , showTitle: Nothing
  , textStyle: Nothing
  , feature: Nothing
  }


instance toolboxEncodeJson ∷ EncodeJson Toolbox where
  encodeJson (Toolbox obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "orient" := obj.orient
        , "x" := obj.x
        , "y" := obj.y
        , "backgroundColor" := obj.backgroundColor
        , "borderColor" := obj.borderColor
        , "borderWidth" := obj.borderWidth
        , "padding" := obj.padding
        , "itemGap" := obj.itemGap
        , "itemSize" := obj.itemSize
        , "color" := obj.color
        , "disableColor" := obj.disableColor
        , "effectiveColor" := obj.effectiveColor
        , "showTitle" := obj.showTitle
        , "textStyle" := obj.textStyle
        , "feature" := obj.feature
        ]

instance toolboxDecodeJson ∷ DecodeJson Toolbox where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , orient: _
        , x: _
        , y: _
        , backgroundColor: _
        , borderColor: _
        , borderWidth: _
        , padding: _
        , itemGap: _
        , itemSize: _
        , color: _
        , disableColor: _
        , effectiveColor: _
        , showTitle: _
        , textStyle: _
        , feature: _ }
        <$> (o .? "show")
        <*> (o .? "orient")
        <*> (o .? "x")
        <*> (o .? "y")
        <*> (o .? "backgroundColor")
        <*> (o .? "borderColor")
        <*> (o .? "borderWidth")
        <*> (o .? "padding")
        <*> (o .? "itemGap")
        <*> (o .? "itemSize")
        <*> (o .? "color")
        <*> (o .? "disableColor")
        <*> (o .? "effectiveColor")
        <*> (o .? "showTitle")
        <*> (o .? "textStyle")
        <*> (o .? "feature")
    pure $ Toolbox r

type FeatureRec =
  { mark ∷ Maybe MarkFeature
  , dataZoom ∷ Maybe DataZoomFeature
  , dataView ∷ Maybe DataViewFeature
  , magicType ∷ Maybe MagicTypeFeature
  , restore ∷ Maybe RestoreFeature
  , saveAsImage ∷ Maybe SaveAsImageFeature
  }

newtype Feature
  = Feature FeatureRec

instance featureEncodeJson ∷ EncodeJson Feature where
  encodeJson (Feature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "mark" := obj.mark
        , "dataZoom" := obj.dataZoom
        , "dataView" := obj.dataView
        , "magicType" := obj.magicType
        , "restore" := obj.restore
        , "saveAsImage" := obj.saveAsImage
        ]

instance featureDecodeJson ∷ DecodeJson Feature where
  decodeJson j = do
    o ← decodeJson j
    r ← { mark: _
        , dataZoom: _
        , dataView: _
        , magicType: _
        , restore: _
        , saveAsImage: _ }
        <$> (o .? "mark")
        <*> (o .? "dataZoom")
        <*> (o .? "dataView")
        <*> (o .? "magicType")
        <*> (o .? "restore")
        <*> (o .? "saveAsImage")
    pure $ Feature r


featureDefault ∷ FeatureRec
featureDefault =
  { mark: Nothing
  , dataZoom: Nothing
  , dataView: Nothing
  , magicType: Nothing
  , restore: Nothing
  , saveAsImage: Nothing
  }

type SaveAsImageFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe String
  , "type" ∷ Maybe ImgType
  , lang ∷ Maybe (Array String)
  }

newtype SaveAsImageFeature
  = SaveAsImageFeature SaveAsImageFeatureRec


instance saveAsImageEncodeJson ∷ EncodeJson SaveAsImageFeature where
  encodeJson (SaveAsImageFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        , "type" := obj."type"
        , "lang" := obj.lang
        ]

instance saveAsImageDecodeJson ∷ DecodeJson SaveAsImageFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , title: _
        , "type": _
        , lang: _ }
        <$> (o .? "show")
        <*> (o .? "title")
        <*> (o .? "type")
        <*> (o .? "lang")
    pure $ SaveAsImageFeature r

saveAsImageFeatureDefault ∷ SaveAsImageFeatureRec
saveAsImageFeatureDefault =
  { show: Nothing
  , title: Nothing
  , "type": Nothing
  , lang: Nothing
  }

type RestoreFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe String
  }

newtype RestoreFeature
  = RestoreFeature RestoreFeatureRec


instance restoreFeatureEncodeJson ∷ EncodeJson RestoreFeature where
  encodeJson (RestoreFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        ]

instance restoreFeatureDecodeJson ∷ DecodeJson RestoreFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _, title: _} <$> (o .? "show") <*> (o .? "title")
    pure $ RestoreFeature r

restoreFeatureDefault ∷ RestoreFeatureRec
restoreFeatureDefault =
  { show: Nothing
  , title: Nothing
  }

type DataZoomFeatureTitleRec =
  { dataZoom ∷ String
  , dataZoomReset ∷ String
  }
newtype DataZoomFeatureTitle
  = DataZoomFeatureTitle DataZoomFeatureTitleRec

instance datazoomTitleEncodeJson ∷ EncodeJson DataZoomFeatureTitle where
  encodeJson (DataZoomFeatureTitle obj) =
    encodeJson
      $ SM.fromFoldable
        [ "dataZoom" := obj.dataZoom
        , "dataZoomReset" := obj.dataZoomReset
        ]

instance datazoomTitleDecodeJson ∷ DecodeJson DataZoomFeatureTitle where
  decodeJson j = do
    o ← decodeJson j
    r ← {dataZoom: _ , dataZoomReset: _ } <$> (o .? "dataZoom") <*> (o .? "dataZoomReset")
    pure $ DataZoomFeatureTitle r

type DataZoomFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe DataZoomFeatureTitle
  }

newtype DataZoomFeature
  = DataZoomFeature DataZoomFeatureRec

dataZoomFeatureDefault ∷ DataZoomFeatureRec
dataZoomFeatureDefault =
  { show: Nothing
  , title: Nothing
  }

instance dataZoomFeatureEncodeJson ∷ EncodeJson DataZoomFeature where
  encodeJson (DataZoomFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        ]

instance dataZoomFeatureDecodeJson ∷ DecodeJson DataZoomFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← {show: _ , title: _} <$> (o .? "show") <*> (o .? "title")
    pure $ DataZoomFeature r

type DataViewFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe String
  , readOnly ∷ Maybe Boolean
  , lang ∷ Maybe (Array String)
  }
newtype DataViewFeature
  = DataViewFeature DataViewFeatureRec

dataViewFeatureDefault ∷ DataViewFeatureRec
dataViewFeatureDefault =
  { show: Nothing
  , title: Nothing
  , readOnly: Nothing
  , lang: Nothing
  }

instance dataViewFeatureEncodeJson ∷ EncodeJson DataViewFeature where
  encodeJson (DataViewFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        , "readOnly" := obj.readOnly
        , "lang" := obj.lang
        ]

instance dataViewFeatureDecodeJson ∷ DecodeJson DataViewFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , title: _
        , readOnly: _
        , lang: _ }
        <$> (o .? "show")
        <*> (o .? "title")
        <*> (o .? "readOnly")
        <*> (o .? "lang")
    pure $ DataViewFeature r

type MarkFeatureTitleRec =
  { mark ∷ Maybe String
  , markUndo ∷ String
  , markClear ∷ String
  }

newtype MarkFeatureTitle
  = MarkFeatureTitle MarkFeatureTitleRec


instance mftitleEncodeJson ∷ EncodeJson MarkFeatureTitle where
  encodeJson (MarkFeatureTitle obj) =
    encodeJson
      $ SM.fromFoldable
        [ "mark" := obj.mark
        , "markUndo" := obj.markUndo
        , "markClear" := obj.markUndo
        ]

instance mftitleDecodeJson ∷ DecodeJson MarkFeatureTitle where
  decodeJson j = do
    o ← decodeJson j
    r ← { mark: _
        , markUndo: _
        , markClear: _ }
        <$> (o .? "mark")
        <*> (o .? "markUndo")
        <*> (o .? "markClear")
    pure $ MarkFeatureTitle r

type MarkFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe MarkFeatureTitle
  , lineStyle ∷ Maybe LineStyle
  }

newtype MarkFeature
  = MarkFeature MarkFeatureRec

instance markFeatureEncodeJson ∷ EncodeJson MarkFeature where
  encodeJson (MarkFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        , "lineStyle" := obj.lineStyle
        ]

instance markFeatureDecodeJson ∷ DecodeJson MarkFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , title: _
        , lineStyle: _ }
        <$> (o .? "show")
        <*> (o .? "title")
        <*> (o .? "lineStyle")
    pure $ MarkFeature r

markFeatureDefault ∷ MarkFeatureRec
markFeatureDefault =
  { show: Nothing
  , title: Nothing
  , lineStyle: Nothing
  }

data MagicType
  = MagicLine
  | MagicBar
  | MagicStack
  | MagicTiled
  | MagicForce
  | MagicChord
  | MagicPie
  | MagicFunnel

instance magicTypeEncodeJson ∷ EncodeJson MagicType where
  encodeJson a = encodeJson $ case a of
    MagicLine → "line"
    MagicBar → "bar"
    MagicStack → "stack"
    MagicTiled → "tiled"
    MagicForce → "force"
    MagicChord → "chord"
    MagicPie → "pie"
    MagicFunnel → "funnel"

instance magicTypeDecodeJson ∷ DecodeJson MagicType where
  decodeJson j = do
    str ← decodeJson j
    case str of
      "line" → pure MagicLine
      "bar" → pure MagicBar
      "stack" → pure MagicStack
      "tiled" → pure MagicTiled
      "force" → pure MagicForce
      "chord" → pure MagicChord
      "pie" → pure MagicPie
      "funnel" → pure MagicFunnel
      _ → Left "incorrect magic type"


type MagicTypeFeatureRec =
  { show ∷ Maybe Boolean
  , title ∷ Maybe (SM.StrMap String)
  , option ∷ Maybe Json
  , "type" ∷ Maybe (Array MagicType)
  }

newtype MagicTypeFeature
  = MagicTypeFeature MagicTypeFeatureRec

magicTypeFeatureDefault ∷ MagicTypeFeatureRec
magicTypeFeatureDefault =
  { show: Nothing
  , title: Nothing
  , option: Nothing
  , "type": Nothing
  }

instance magicTypeFeatureEncodeJson ∷ EncodeJson MagicTypeFeature where
  encodeJson (MagicTypeFeature obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "title" := obj.title
        , "option" := obj.option
        , "type" := obj."type"
        ]

instance magicTypeFeatureDecodeJson ∷ DecodeJson MagicTypeFeature where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , title: _
        , option: _
        , "type": _ }
        <$> (o .? "show")
        <*> (o .? "title")
        <*> (o .? "option")
        <*> (o .? "type")
    pure $ MagicTypeFeature r
