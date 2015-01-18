module Lasagna where

import Data.Tuple
import Data.Tuple.Nested
import Data.Foldable
import Data.Array hiding (head, tail, last, init)
import Data.Array.Unsafe hiding (init)
import ECharts.Options.Unsafe

import ECharts.Chart
import Data.Maybe
import Utils

series =
  let numbers = 0 .. 29
      mapfn = \i ->
        {
          name: "chinese word",
          "type": "pie",
          itemStyle:
          {
            normal:
            {
              label: {show: i > 28},
              labelLine: {show: i > 28, length: 20}
            }
          },
          radius: [i * 4 + 40, i * 4 + 43],
          "data":
          [
            {value: i * 128 + 80, name: "Chrome"},
            {value: i * 64 + 160, name: "Firefox"},
            {value: i * 32 + 320, name: "Safari"},
            {value: i * 16 + 640, name: "IE9+"},
            {value: i * 8 + 1280, name: "IE8-"}
          ]
        }
      series = mapfn <$> numbers
      mp =
        {
          name: "chinese word",
          "type": "pie",
          "data":
          [
          ],
          markPoint:
          {
            symbol: "emptyCircle",
            symbolSize: (head series).radius # head,
            effect:
            {
              show: true,
              scaleSize: 12,
              color: "rgba(250, 250, 50, 0.8)",
              shadowBlur: 10,
              period: 30
            },
            "data": [{x: "50%", y: "50%"}]
          }
        }
  in mp /\ Merge series

options =
  {
    "title":
    {
      "text": "Title",
      "subtext": "Subtitle",
      "x": "right",
      "y": "bottom"
    },
    "tooltip":
    {
      "trigger": "item",
      "formatter": "{a} <br />{b} : {c} ({d}%)"
    },
    "legend":
    {
      "orient": "vertical",
      "x": "left",
      "data": ["Chrome", "Firefox", "Safari", "IE9+", "IE8-"]
    },
    "toolbox":
    {
      "show": true,
      "feature":
      {
        "mark": {"show": true},
        "dataView": {"show": true, "readOnly": false},
        "restore": {"show": true},
        "saveAsImage": {"show": true}
      }
    },
    "calculable": false,
    "series": series
  }

lasagna id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOptionUnsafe options true

  return unit
