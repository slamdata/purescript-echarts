module Mix2Safe where

import Control.Monad.Eff
import Utils

import Data.Maybe
import Data.Tuple

import ECharts.Chart
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Type
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item


options :: Option
options = Option $ emptyOptions {
  tooltip = Just $ Tooltip emptyTooltip {trigger = Just TriggerAxis},
  toolbox = Just $ Toolbox emptyToolbox {
    show = Just true,
    y = Just YBottom ,
    feature = Just $ Feature {
      mark: Just $ MarkFeature {
         show: Just true,
         title: Nothing,
         lineStyle: Nothing
         },
      dataView: Just $ DataViewFeature {
        show: Just true,
        title: Nothing,
        readOnly: Just true,
        lang: Nothing
        },
      magicType: Just $ MagicTypeFeature {
        show: Just true,
        title: Nothing,
        option: Nothing,
        "type": Just $ [MagicLine, MagicBar, MagicStack, MagicTiled]
        },
      restore: Just $ RestoreFeature {
        show: Just true,
        title: Nothing
        },
      saveAsImage: Just $ SaveAsImageFeature {
        show: Just true,
        title: Nothing,
        "type": Nothing,
        "lang": Nothing
        },
      dataZoom: Nothing
      }
    },
  calculable = Just true,
  legend = Just $ Legend $ emptyLegend {
    "data" = Just $ (LegendString <$> ["直接访问","邮件营销","联盟广告","视频广告",
                                         "搜索引擎","百度","谷歌","必应","其他"])
    
    },
  xAxis = Just $ OneAxis $ Axis $ emptyAxis {
    "type" = Just CategoryAxis,
    "splitLine" = Just $ AxisSplitLine {
      "show": Just false,
      "onGap": Nothing,
      "lineStyle": Nothing
      },
    "data" = Just $ (CommonAxisData <$>
                     ["周一","周二","周三","周四","周五","周六","周日"])
    },
  yAxis = Just $ OneAxis $ Axis $ emptyAxis {
    "type" = Just ValueAxis,
    "position" = Just RightAxis
    },
  series = Just $ [
    Series $ (emptySeries Bar){
       "name" = Just "直接访问",
       "data" = Just $ ((Value <<< Simple) <$> [320, 332, 301, 334, 390, 330, 320])
       },
    Series $ (emptySeries Bar) {
      "name" = Just "邮件营销",
      "tooltip" = Just $ Tooltip $ emptyTooltip {trigger = Just TriggerItem},
      "stack" = Just "广告",
      "data" = Just $ ((Value <<< Simple) <$> [120, 132, 101, 134, 90, 230, 210])
      },
    Series $ (emptySeries Bar) {
      "name" = Just "联盟广告",
      "tooltip" = Just $ Tooltip $ emptyTooltip {trigger = Just TriggerItem},
      "stack" = Just "广告",
      "data" = Just $ ((Value <<< Simple) <$> [220, 182, 191, 234, 290, 330, 310])
      },
    Series $ (emptySeries Bar) {
      "name" = Just "视频广告",
      "tooltip" = Just $ Tooltip $ emptyTooltip {trigger = Just TriggerItem},
      "stack" = Just "广告",
      "data" = Just $ ((Value <<< Simple) <$> [150, 232, 201, 154, 190, 330, 410])
      },
    Series $ (emptySeries Line) {
      "data" = Just $ ((Value <<< Simple) <$>
                       [862, 1018, 964, 1026, 1679, 1600, 1570])
      },
    Series $ (emptySeries Pie) {
      "name" = Just "搜索引擎细分",
      "center" = Just (Tuple 160 130),
      "radius" = Just $ Rs {inner: 0, outer: 50},
      "itemStyle" = Just $ ItemStyle {
        "emphasis": Nothing,
        "normal": Just $ IStyle $ emptyIStyle {
          "labelLine" = Just $ ItemLabelLine {
             "show": Just true,
             "length" : Just 20,
             "lineStyle": Nothing
             }
          }
        },
      "tooltip" = Just $ Tooltip $ emptyTooltip {
        trigger = Just TriggerItem,
        formatter = Just $ Template "{a} <br/>{b} : {c} ({d}%)"
        },
      "data" = Just $ [
        Dat $ (emptyData (Simple 1047)){
           "name" = Just "百度"
           },
        Dat $ (emptyData (Simple 264)){
          "name" = Just "谷歌"
          },
        Dat $ (emptyData (Simple 145)){
          "name" = Just "必应"
          },
        Dat $ (emptyData (Simple 102)){
          "name" = Just "其他"
          }
        ]
      }
    
    ]
  
  }


mix2safe id = do
  chart <- getElementById id
           >>= init Nothing

  setOption options true chart
  return unit
