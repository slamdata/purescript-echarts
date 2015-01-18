module Bar13 where
import Data.Array hiding (init)
import Data.Maybe
import Data.Tuple
import Data.Tuple.Nested

import ECharts.Chart
import ECharts.Options.Unsafe
import Data.Maybe
import Utils

options =
  {
    title:
      {
        text: "Another chinese stuff",
        subtext: "Subtitle here"
      },
    tooltip:
      {
        trigger: "axis",
        axisPointer:
          {
            "type": "cross",
            lineStyle:
              {
                "type": "dashed",
                width: 1
              }
          },
        formatter: (\p ->
                     p.seriesName <> " : [ " <>
                     fromMaybe "" (p.value !! 0) <> ", " <>
                     fromMaybe "" (p.value !! 1) <> "]")
      },
    legend:
      {
        "data": ["数据", "数据"]
      },
    toolbox:
      {
        show: true,
        feature:
          {
            mark: {show: true},
            dataView: {show: true, readOnly: false},
            magicType: {show: true, "type": ["line", "bar"]},
            restore: {show: true},
            saveAsImage: {show: true}
          }
      },
    calculable: true,
    xAxis:
    [
      {
        "type": "value"
      }
    ],
    yAxis:
    [{
        "type": "value",
        axisLine:
          {
            lineStyle: {color: "#dc143c"}
          }
     }],
    series:
      (
      Nothing /\
      {
        name: "数据",
        "type": "bar",
        "data":
        [
          [1.5, 10],
          [5, 7],
          [8, 8],
          [12, 6],
          [11, 12],
          [16, 9],
          [14, 6],
          [17, 4],
          [19, 9]
        ],
        markPoint:
         {
           "data":
             (
               {
                 "type": "max",
                 name: "最大值",
                 symbol: "emptyCircle",
                 itemStyle:
                   {
                     normal:
                       {
                         color: "#dc143c",
                         label: {position: "top"}
                       }
                   }
               } /\
               {
                 "type": "min",
                 name: "最小值",
                 symbol: "emptyCircle",
                 itemStyle: {normal: {color: "#dc143c", label: {position: "top"}}}
               } /\
               {
                 "type": "max",
                 name: "最大值",
                 valueIndex: 0,
                 symbol: "emptyCircle",
                 itemStyle: {normal: {color: "#1e90ff", label: {position: "right"}}}
               } /\
               {
                 "type": "min",
                 name: "最小值",
                 valueIndex: 0,
                 symbol: "emptyCircle",
                 itemStyle: {normal: {color: "#1e90ff", label: {position: "right"}}}
               }
             )
         },
        makrLine:
         {
           "data":
             (
               {
                 "type": "max",
                 name: "max",
                 itemStyle: {normal: {color: "#dc143c"}}
               } /\
               {
                 "type": "min",
                 name: "min",
                 itemStyle: {normal: {color: "#dc143c"}}
               } /\
               {
                 "type": "average",
                 name: "average",
                 itemStyle: {normal: {color: "#dc143c"}}
               } /\
               {
                 "type": "max",
                 name: "max",
                 valueIndex: 0,
                 itemStyle: {normal: {color: "#1e90ff"}}
               } /\
               {
                 "type": "min",
                 name: "min",
                 valueIndex: 0,
                 itemStyle: {normal: {color: "#1e90ff"}}
               } /\
               {
                 "type": "average",
                 name: "average",
                 itemStyle: {normal: {color: "#1e90ff"}}
               }
             )
         }
      } /\
      {
       name: "bar",
       "type": "bar",
       barHeight: 10,
       "data":
         [
           [1, 2],
           [2, 3],
           [4, 4],
           [7, 5],
           [11, 11],
           [18, 15]
         ]
     }
    )
  }

bar13 id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOptionUnsafe options true

  return unit
