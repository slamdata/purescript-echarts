module ECharts.DataZoom where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Color (Color)
import ECharts.Coords (XPos, YPos, Orient)

type DataZoomRec =
  { show ∷ Maybe Boolean
  , orient ∷ Maybe Orient
  , x ∷ Maybe XPos
  , y ∷ Maybe YPos
  , width ∷ Maybe Number
  , height ∷ Maybe Number
  , backgroundColor ∷ Maybe Color
  , dataBackgroundColor ∷ Maybe Color
  , fillerColor ∷ Maybe Color
  , handleColor ∷ Maybe Color
  , xAxisIndex ∷ Maybe (Array Number)
  , yAxisIndex ∷ Maybe (Array Number)
  , start ∷ Maybe Number
  , end ∷ Maybe Number
  , showDetail ∷ Maybe Boolean
  , realtime ∷ Maybe Boolean
  , zoomlock ∷ Maybe Boolean
  }

newtype DataZoom
  = DataZoom DataZoomRec

instance dataZoomEncodeJson ∷ EncodeJson DataZoom where
  encodeJson (DataZoom obj) =
    encodeJson
      $ SM.fromFoldable
        [ "show" := obj.show
        , "orient" := obj.orient
        , "x" := obj.x
        , "y" := obj.y
        , "width" := obj.width
        , "height" := obj.height
        , "backgroundColor" := obj.backgroundColor
        , "dataBackgroundColor" := obj.dataBackgroundColor
        , "fillerColor" := obj.fillerColor
        , "handleColor" := obj.handleColor
        , "xAxisIndex" := obj.xAxisIndex
        , "yAxisIndex" := obj.yAxisIndex
        , "start" := obj.start
        , "end" := obj.end
        , "showDetail" := obj.showDetail
        , "realtime" := obj.realtime
        , "zoomlock" := obj.zoomlock
        ]


instance dataZoomDecodeJson ∷ DecodeJson DataZoom where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , orient: _
        , x: _
        , y: _
        , width: _
        , height: _
        , backgroundColor: _
        , dataBackgroundColor: _
        , fillerColor: _
        , handleColor: _
        , xAxisIndex: _
        , yAxisIndex: _
        , start: _
        , end: _
        , showDetail: _
        , realtime: _
        , zoomlock: _ }
        <$> (o .? "show")
        <*> (o .? "orient")
        <*> (o .? "x")
        <*> (o .? "y")
        <*> (o .? "width")
        <*> (o .? "height")
        <*> (o .? "backgroundColor")
        <*> (o .? "dataBackgroundColor")
        <*> (o .? "fillerColor")
        <*> (o .? "handleColor")
        <*> (o .? "xAxisIndex" )
        <*> (o .? "yAxisIndex" )
        <*> (o .? "start")
        <*> (o .? "end")
        <*> (o .? "showDetail")
        <*> (o .? "realtime")
        <*> (o .? "zoomlock")
    pure $ DataZoom r

dataZoomDefault ∷ DataZoomRec
dataZoomDefault =
  { show: Nothing
  , orient: Nothing
  , x: Nothing
  , y: Nothing
  , width: Nothing
  , height: Nothing
  , backgroundColor: Nothing
  , dataBackgroundColor: Nothing
  , fillerColor: Nothing
  , handleColor: Nothing
  , xAxisIndex: Nothing
  , yAxisIndex: Nothing
  , start: Nothing
  , end: Nothing
  , showDetail: Nothing
  , realtime: Nothing
  , zoomlock: Nothing
  }
