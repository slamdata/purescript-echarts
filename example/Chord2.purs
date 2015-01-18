module Chord2 where
import Data.Array hiding (init)
import Data.String (indexOf, replace)
import Data.Tuple.Nested
import Data.Argonaut

import Data.Either
import Data.Maybe 
import qualified Data.StrMap as M
import Control.Alt 


import ECharts.Chart
import ECharts.Options.Unsafe
import Data.Maybe
import Utils
options =
  {
    color : [
        "#FBB367","#80B1D2","#FB8070","#CC99FF","#B0D961",
        "#99CCCC","#BEBBD8","#FFCC99","#8DD3C8","#FF9999",
        "#CCEAC4","#BB81BC","#FBCCEC","#CCFF66","#99CC66",
        "#66CC66","#FF6666","#FFED6F","#ff7f50","#87cefa"
    ],
    title : {
        text : "中东地区的敌友关系",
        subtext: "数据来自财新网",
        sublink: "http://international.caixin.com/2013-09-06/100579154.html",
        x:"right",
        y:"bottom"
    },
    toolbox: {
        show : true,
        feature : {
            restore : {show: true},
            magicType: {show: true, type: ["force", "chord"]},
            saveAsImage : {show: true}
        }
    },
    tooltip: {
      trigger: "item",
      -- TODO : Write a wrapper function from params to {key :: Maybe value}
      formatter: (\params -> 
                   fromMaybe ""
                   (do
                     obj <- toObject params
                     name <- M.lookup "name" obj >>= toString
                     seriesName <- M.lookup "seriesName" obj >>= toString
                     return $ replace "-" " " name <> seriesName <> " "

                   <|> do
                       toObject params
                         >>= M.lookup "data"
                         >>= toObject
                         >>= M.lookup "id"
                         >>= toString
                   )
                 )

    },
    legend : {
        "data" : [
            "美国",
            "叙利亚反对派",
            "阿萨德",
            "伊朗",
            "塞西",
            "哈马斯",
            "以色列",
            "穆斯林兄弟会",
            "基地组织",
            "俄罗斯",
            "黎巴嫩什叶派",
            "土耳其",
            "卡塔尔",
            "沙特",
            "黎巴嫩逊尼派",
            "",
            "支持",
            "反对",
            "未表态"
        ],
        orient: "vertical",
        x: "left"
    },
    series:
    (
     {
       "name": "支持",
       "type": "chord",
       "showScaleText": false,
       "clockWise": false,
       "data": [
         {"name": "美国"},
         {"name": "叙利亚反对派"},
         {"name": "阿萨德"},
         {"name": "伊朗"},
         {"name": "塞西"},
         {"name": "哈马斯"},
         {"name": "以色列"},
         {"name": "穆斯林兄弟会"},
         {"name": "基地组织"},
         {"name": "俄罗斯"},
         {"name": "黎巴嫩什叶派"},
         {"name": "土耳其"},
         {"name": "卡塔尔"},
         {"name": "沙特"},
         {"name": "黎巴嫩逊尼派"}
         ],
       "matrix": [
         [0,100,0,0,0,0,100,0,0,0,0,0,0,0,0],
         [10,0,0,0,0,10,10,0,10,0,0,10,10,10,10],
         [0,0,0,10,0,0,0,0,0,10,10,0,0,0,0],
         [0,0,100,0,0,100,0,0,0,0,100,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,10,0],
         [0,100,0,10,0,0,0,0,0,0,0,0,10,0,0],
         [10,100,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,10,10,0,0],
         [0,100,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,100,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,100,10,0,0,0,0,0,0,0,0,0,0,0],
         [0,100,0,0,0,0,0,100,0,0,0,0,0,0,0],
         [0,100,0,0,0,100,0,100,0,0,0,0,0,0,0],
         [0,100,0,0,100,0,0,0,0,0,0,0,0,0,100],
         [0,100,0,0,0,0,0,0,0,0,0,0,0,10,0]
         ]
     } /\
     {
       "name": "反对",
       "type": "chord",
       "insertToSerie": "支持",
       "data": [
         {"name": "美国"},
         {"name": "叙利亚反对派"},
         {"name": "阿萨德"},
         {"name": "伊朗"},
         {"name": "塞西"},
         {"name": "哈马斯"},
         {"name": "以色列"},
         {"name": "穆斯林兄弟会"},
         {"name": "基地组织"},
         {"name": "俄罗斯"},
         {"name": "黎巴嫩什叶派"},
         {"name": "土耳其"},
         {"name": "卡塔尔"},
         {"name": "沙特"},
         {"name": "黎巴嫩逊尼派"}
         ],
       "matrix": [
         [0,0,100,100,0,100,0,0,100,0,0,0,0,0,0],
         [0,0,0,10,0,0,0,0,0,10,10,0,0,0,0],
         [10,0,0,0,0,0,10,10,10,0,0,10,10,0,10],
         [10,100,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,10,0,100,0,0,0,10,10,0,0],
         [10,0,0,0,100,0,10,0,0,0,0,0,0,0,0],
         [0,0,100,0,0,100,0,0,0,0,0,0,0,0,0],
         [0,0,100,0,10,0,0,0,0,0,0,0,0,10,0],
         [10,0,100,0,0,0,0,0,0,0,0,0,0,100,0],
         [0,100,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,100,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,100,0,100,0,0,0,0,0,0,0,0,0,0],
         [0,0,100,0,100,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,100,10,0,0,0,0,0,0],
         [0,0,100,0,0,0,0,0,0,0,0,0,0,0,0]
         ]
     } /\
     {
       "name": "未表态",
       "type": "chord",
       "insertToSerie": "支持",
       "data": [
         {"name": "美国"},
         {"name": "叙利亚反对派"},
         {"name": "阿萨德"},
         {"name": "伊朗"},
         {"name": "塞西"},
         {"name": "哈马斯"},
         {"name": "以色列"},
         {"name": "穆斯林兄弟会"},
         {"name": "基地组织"},
         {"name": "俄罗斯"},
         {"name": "黎巴嫩什叶派"},
         {"name": "土耳其"},
         {"name": "卡塔尔"},
         {"name": "沙特"},
         {"name": "黎巴嫩逊尼派"}
         ],
       "matrix": [
         [0,0,0,0,100,0,0,100,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [10,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [10,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
         ]
     }     

    )
        
  }

chord2 id = do
  chart <- getElementById id
           >>= init Nothing
           >>= setOptionUnsafe options true

  return unit
