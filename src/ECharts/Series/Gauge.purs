module ECharts.Series.Gauge where

import ECharts.Prelude

import Data.StrMap as SM

import ECharts.Common (PercentOrPixel)
import ECharts.Color (Color)
import ECharts.Style.Line (LineStyle)
import ECharts.Style.Text (TextStyle)
import ECharts.Formatter (Formatter)

type PointerRec =
  { length ∷ Maybe Number
  , width ∷ Maybe Number
  , color ∷ Maybe Color
  }

newtype Pointer = Pointer PointerRec

pointerDefault ∷ PointerRec
pointerDefault =
  { length: Nothing
  , width: Nothing
  , color: Nothing
  }

instance pointerEncodeJson ∷ EncodeJson Pointer where
  encodeJson (Pointer p) =
    encodeJson
      $ SM.fromFoldable
          [ "length" := p.length
          , "width" := p.width
          , "color" := p.color
          ]

instance pointerDecodeJson ∷ DecodeJson Pointer where
  decodeJson j = do
    o ← decodeJson j
    r ← { length: _
        , width: _
        , color: _ }
        <$> (o .? "length")
        <*> (o .? "width")
        <*> (o .? "color")
    pure $ Pointer r

type SplitLineRec =
  { show ∷ Maybe Boolean
  , length ∷ Maybe Number
  , lineStyle ∷ Maybe LineStyle
  }

newtype SplitLine
  = SplitLine SplitLineRec

splitLineDefault ∷ SplitLineRec
splitLineDefault =
  { show: Nothing
  , length: Nothing
  , lineStyle: Nothing
  }

instance splitLineEncodeJson ∷ EncodeJson SplitLine where
  encodeJson (SplitLine sl) =
    encodeJson
      $ SM.fromFoldable
          [ "show" := sl.show
          , "length" := sl.length
          , "lineStyle" := sl.lineStyle
          ]

instance splitLineDecodeJson ∷ DecodeJson SplitLine where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , length: _
        , lineStyle: _ }
        <$> (o .? "show")
        <*> (o .? "length")
        <*> (o .? "lineStyle")
    pure $ SplitLine r


type GaugeDetailRec =
  { show ∷ Maybe Boolean
  , backgroundColor ∷ Maybe Color
  , borderWidth ∷ Maybe Number
  , borderColor ∷ Maybe Color
  , width ∷ Maybe Number
  , height ∷ Maybe Number
  , offsetCenter ∷ Maybe (Tuple PercentOrPixel PercentOrPixel)
  , formatter ∷ Maybe Formatter
  , textStyle ∷ Maybe TextStyle
  }

newtype GaugeDetail
  = GaugeDetail GaugeDetailRec

gaugeDetailDefault ∷ GaugeDetailRec
gaugeDetailDefault =
  { show: Nothing
  , backgroundColor: Nothing
  , borderWidth: Nothing
  , borderColor: Nothing
  , width: Nothing
  , height: Nothing
  , offsetCenter: Nothing
  , formatter: Nothing
  , textStyle: Nothing
  }


instance gaugeDetailEncodeJson ∷ EncodeJson GaugeDetail where
  encodeJson (GaugeDetail gd) =
    encodeJson
      $ SM.fromFoldable
          [ "show" := gd.show
          , "backgroundColor" := gd.backgroundColor
          , "borderWidth" := gd.borderWidth
          , "borderColor" := gd.borderColor
          , "width" := gd.width
          , "height" := gd.height
          , "offsetCenter" := gd.offsetCenter
          , "formatter" := gd.formatter
          , "textStyle" := gd.textStyle
          ]

instance gaugeDetailDecodeJson ∷ DecodeJson GaugeDetail where
  decodeJson j = do
    o ← decodeJson j
    r ← { show: _
        , backgroundColor: _
        , borderWidth: _
        , borderColor: _
        , width: _
        , height: _
        , offsetCenter: _
        , formatter: _
        , textStyle: _ }
        <$> (o .? "show")
        <*> (o .? "backgroundColor")
        <*> (o .? "borderWidth")
        <*> (o .? "borderColor")
        <*> (o .? "width")
        <*> (o .? "height")
        <*> (o .? "offsetCenter")
        <*> (o .? "formatter")
        <*> (o .? "textStyle")
    pure $ GaugeDetail r
