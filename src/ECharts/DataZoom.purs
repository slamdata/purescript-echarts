module ECharts.DataZoom where

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode
import Data.Maybe
import Data.StrMap (fromList)
import ECharts.Color
import ECharts.Coords



newtype DataZoom =
  DataZoom {
    "show" :: Maybe Boolean,
    "orient" :: Maybe Orient,
    "x" :: Maybe XPos,
    "y" :: Maybe YPos,
    "width" :: Maybe Number,
    "height" :: Maybe Number,
    "backgroundColor" :: Maybe Color,
    "dataBackgroundColor" :: Maybe Color,
    "fillerColor" :: Maybe Color,
    "handleColor" :: Maybe Color,
    "xAxisIndex" :: Maybe [Number],
    "yAxisIndex" :: Maybe [Number],
    "start" :: Maybe Number,
    "end" :: Maybe Number,
    "showDetail" :: Maybe Boolean,
    "realtime" :: Maybe Boolean,
    "zoomlock" :: Maybe Boolean
    }

instance dataZoomEncodeJson :: EncodeJson DataZoom where
  encodeJson (DataZoom obj) =
    fromObject $ fromList $
    [
      "show" := obj.show,
      "orient" := obj.orient,
      "x" := obj.x,
      "y" := obj.y,
      "width" := obj.width,
      "height" := obj.height,
      "backgroundColor" := obj.backgroundColor,
      "dataBackgroundColor" := obj.dataBackgroundColor,
      "fillerColor" := obj.fillerColor,
      "handleColor" := obj.handleColor,
      "xAxisIndex" := obj.xAxisIndex,
      "yAxisIndex" := obj.yAxisIndex,
      "start" := obj.start,
      "end" := obj.end,
      "showDetail" := obj.showDetail,
      "realtime" := obj.realtime,
      "zoomlock" := obj.zoomlock
    ]
dataZoomDefault = {
  show: Nothing,
  orient: Nothing,
  x: Nothing,
  y: Nothing,
  width: Nothing,
  height: Nothing,
  backgroundColor: Nothing,
  dataBackgroundColor: Nothing,
  fillerColor: Nothing,
  handleColor: Nothing,
  xAxisIndex: Nothing,
  yAxisIndex: Nothing,
  start: Nothing,
  end: Nothing,
  showDetail: Nothing,
  realtime: Nothing,
  zoomlock: Nothing
  }
