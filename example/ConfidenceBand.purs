module ConfidenceBand where

import DOM (DOM())
import DOM.Node.Types (ElementId(..))

import Prelude
import Control.Monad.Eff.Console (print, CONSOLE())
import Data.Tuple.Nested
import Data.Tuple
import Data.Maybe
import Data.Array (zipWith, head)
import Data.Function
import Data.Foldable
import Data.Argonaut.Core
import Data.Argonaut.Combinators ((.?))
import Data.Either (Either(..), either)
import Utils

import ECharts.Chart
import ECharts.Options
import ECharts.Tooltip
import ECharts.Toolbox
import ECharts.Coords
import ECharts.Legend
import ECharts.Axis
import ECharts.Series
import ECharts.Item.Data
import ECharts.Item.Value
import ECharts.Common
import ECharts.Formatter
import ECharts.Style.Item
import ECharts.Style.Text
import ECharts.Style.Line
import ECharts.Style.Area
import ECharts.Color
import ECharts.Title
import ECharts.Symbol
import ECharts.Utils
import Control.Monad.Eff (Eff())
import ECharts.Effects


simpleData = Value <<< Simple


toolTipFomatter :: (Number -> String) -> (Array FormatParams -> String) 
toolTipFomatter func = formatter
  where
  formatter :: Array FormatParams -> String
  formatter params =
    (getName params) ++
      (foldr (++) "" $ map (append "<br/>") $ 
        zipWith (\a b -> a ++ ": " ++ b) (map getSeriesName params) (map getValues params))

  getName :: Array FormatParams -> String 
  getName params = case head params of
    Just param -> fromMaybe "" (getStrField param "name")
    _ -> ""

  getSeriesName :: FormatParams -> String 
  getSeriesName en = 
    fromMaybe "" (getStrField en "seriesName")
  
  getValues :: FormatParams -> String 
  getValues en = 
    transform func (getNumField en "value")

  transform :: (Number -> String) -> Maybe Number -> String
  transform func num = case num of
    Just n -> func n
    _ -> ""

  getStrField :: FormatParams -> String -> Maybe String
  getStrField entry f = case (toObject entry) of
    Just o -> hush $ o .? f
    _ -> Just ""

  getNumField :: FormatParams -> String -> Maybe Number
  getNumField entry f = case (toObject entry) of
    Just o -> hush $ o .? f
    _ -> Nothing
  
  hush :: forall a b. Either a b -> Maybe b
  hush = either (const Nothing) Just


options :: Option
options = Option $ optionDefault {
  title = Just $ Title titleDefault {
    text = Just "Confidence Band",
    subtext = Just "confidence band, tooltip formatter",
    x = Just XCenter,
    textStyle = Just $ TextStyle textStyleDefault {
      fontFamily = Just "Palatino, Georgia, serif"
      }
    },
  tooltip = Just $ Tooltip tooltipDefault {
    trigger = Just TriggerAxis,
    formatter = Just $ FormatFuncNonEff $ 
      toolTipFomatter (numeralFormatterWithValMnplt dataBase 1.0 "0.00"),
    textStyle = Just $ TextStyle textStyleDefault {
      fontFamily = Just "Palatino, Georgia, serif",
      fontSize = Just 12.0
      },
    axisPointer = Just $ TooltipAxisPointer tooltipAxisPointerDefault {
      "type" = Just $ LinePointer,
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(170,170,170,0.8)",
        width = Just 1.5,
        "type" = Just $ Solid
        }
      }
    },
  toolbox = Just $ Toolbox $ toolboxDefault {
    show = Just false,
    feature = Just $ Feature $ featureDefault 
    },
  calculable = Just true,
  xAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just CategoryAxis,
    boundaryGap = Just $ CatBoundaryGap false,
    axisLine = Just $ AxisLine axisLineDefault {
      lineStyle = Just $ AxisLineStyle axisLineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    axisTick = Just $ AxisTick axisTickDefault {
      length = Just $ 2.0,
      lineStyle = Just $ LineStyle lineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    splitLine = Just $ AxisSplitLine axisSplitLineDefault {
      show = Just false
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      formatter = Just $ StringFormatFunc (dateTimeFormatter "MMM-DD"),
      textStyle = Just $ TextStyle textStyleDefault {
        fontFamily = Just "Palatino, Georgia, serif"
        }
      },
    "data" = Just $ CommonAxisData <$> dataDate
    },
  yAxis = Just $ OneAxis $ Axis $ axisDefault {
    "type" = Just ValueAxis,
    axisLine = Just $ AxisLine axisLineDefault {
      lineStyle = Just $ AxisLineStyle axisLineStyleDefault {
        color = Just "rgba(184,184,184,0.8)",
        width = Just 1.0
        }
      },
    splitLine = Just $ AxisSplitLine axisSplitLineDefault {
      show = Just false
      },
    splitArea = Just $ AxisSplitArea axisSplitAreaDefault {
      areaStyle = Just $ AreaStyle areaStyleDefault { 
                color = Just $ SimpleColor "rgba(255,255,255,1)"           
        }
      },
    axisLabel = Just $ AxisLabel axisLabelDefault {
      formatter = Just $ NumberFormatFunc (
        numeralFormatterWithValMnplt dataBase 1.0 "0.00"),
      textStyle = Just $ TextStyle textStyleDefault {
       fontFamily = Just "Palatino, Georgia, serif"
       }
      }
    },

  series = Just $ Just <$> [
    LineSeries {
      common: universalSeriesDefault {
        name = Just "value",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            color = Just $ SimpleColor "rgba(0,119,215,1)"
            }     
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ NoSymbol,
        smooth = Just false,
        "data" = Just $ simpleData <$> ((\n->n-dataBase) <$> dataVal)
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "lower boundary",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            lineStyle = Just $ LineStyle lineStyleDefault {
              width= Just 0.0
              },
            areaStyle = Just $ AreaStyle areaStyleDefault { 
                color = Just $ SimpleColor "rgba(255,255,255,1)"           
              }            }     
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ NoSymbol,
        smooth = Just false,
        "data" = Just $ simpleData <$> ((\n->n-dataBase) <$> dataL)
        }
      },
    LineSeries {
      common: universalSeriesDefault {
        name = Just "upper boundary",
        itemStyle = Just $ ItemStyle itemStyleDefault {
          normal = Just $ IStyle istyleDefault {
            lineStyle = Just $ LineStyle lineStyleDefault {
              width= Just 0.0
              },
            areaStyle = Just $ AreaStyle areaStyleDefault { 
                color = Just $ SimpleColor "#ebf6ff"           
              }
            }     
          }
        },
      lineSeries: lineSeriesDefault {
        symbol = Just $ NoSymbol,
        smooth = Just false,
        "data" = Just $ simpleData <$>  ((\n->n-dataBase) <$> dataU)
        }
      }
    ]
  }


confidenceBand :: forall eff. 
  ElementId -> 
  Eff ( echartSetOption :: ECHARTS_OPTION_SET
        , echartInit :: ECHARTS_INIT
        , console :: CONSOLE
        , dom :: DOM
        | eff
        ) Unit
confidenceBand id = do
  mbEl <- getElementById id
  case mbEl of
    Nothing -> print "incorrect id in confidence-band"
    Just el -> do
      chart <- init Nothing el
      chart' <- setOption options true chart
      pure unit

dataDate = 
  [ "2012-08-28 ", "2012-08-29 ", "2012-08-30 ", "2012-08-31 ", "2012-09-01 ", "2012-09-02", "2012-09-03 ", "2012-09-04 "
  , "2012-09-05 ", "2012-09-06 ", "2012-09-07 ", "2012-09-08", "2012-09-09 ", "2012-09-10 ", "2012-09-11 ", "2012-09-12 "
  , "2012-09-13 ", "2012-09-14", "2012-09-15 ", "2012-09-16 ", "2012-09-17 ", "2012-09-18 ", "2012-09-19 ", "2012-09-20"
  , "2012-09-21 ", "2012-09-22 ", "2012-09-23 ", "2012-09-24 ", "2012-09-25 ", "2012-09-26", "2012-09-27 ", "2012-09-28 "
  , "2012-09-29 ", "2012-09-30 ", "2012-10-01 ", "2012-10-02", "2012-10-03 ", "2012-10-04 ", "2012-10-05 ", "2012-10-06 "
  , "2012-10-07 ", "2012-10-08", "2012-10-09 ", "2012-10-10 ", "2012-10-11 ", "2012-10-12 ", "2012-10-13 ", "2012-10-14"
  , "2012-10-15 ", "2012-10-16 ", "2012-10-17 ", "2012-10-18 ", "2012-10-19 ", "2012-10-20", "2012-10-21 ", "2012-10-22 "
  , "2012-10-23 ", "2012-10-24 ", "2012-10-25 ", "2012-10-26", "2012-10-27 ", "2012-10-28 ", "2012-10-29 ", "2012-10-30 "
  , "2012-10-31 ", "2012-11-01", "2012-11-02 ", "2012-11-03 ", "2012-11-04 ", "2012-11-05 ", "2012-11-06 ", "2012-11-07"
  , "2012-11-08 ", "2012-11-09 ", "2012-11-10 ", "2012-11-11 ", "2012-11-12 ", "2012-11-13", "2012-11-14 ", "2012-11-15 "
  , "2012-11-16 ", "2012-11-17 ", "2012-11-18 ", "2012-11-19", "2012-11-21 ", "2012-11-28 ", "2012-12-05 ", "2012-12-12 "
  , "2012-12-19 ", "2012-12-27", "2012-12-31" ]
dataVal = 
  [ -1.1618426259, -0.5828247293, -0.3790770636, -0.2792926002, -0.2461165469, -0.2017354137, -0.1457476871
  , -0.0026109730, -0.0080692734, -0.0296490933, 0.0013173970, -0.0117649838, 0.0059394263, -0.0115565898, 0.0041183019
  , 0.0353559544, 0.0070046011, -0.0004251807, -0.0035461023, 0.0077978890, 0.0025402723, -0.0053173810, -0.0075841521
  , -0.0391388721, 0.0075430252, 0.1850284663, 0.0766295960, -0.0314292271, -0.0232608674, -0.0196861500, -0.0310196816
  , -0.0758746967, 0.0233974572, 0.0110735790, -0.0020948220, -0.1083707096, -0.1098258972, -0.0872970297, -0.0761992047
  , -0.0416654249, -0.0410128962, -0.0214289042, 0.2430880604, 0.3472823479, 0.3360734074, -0.0463648355, -0.0867009379
  , -0.1288672826, -0.1474426821, -0.1502405066, -0.1203765529, -0.0649122919, -0.0155255620, -0.0060513570, 0.0003154213
  , -0.0063018298, -0.0042948340, -0.0053400832, 0.0070057212, 0.0082121656, 0.0141422884, 0.0041613553, -0.0013614287
  , -0.0052144933, 0.0078904741, 0.0099598702, 0.0001146029, 0.0047572651, 0.0062045570, 0.0115231406, -0.0032634994
  , -0.0108985452, -0.0092766813, 0.0095972086, -0.0111809358, -0.0023572296, 0.0084213775, 0.0107446453, 0.0094577920
  , 0.0031194779, -0.0115128213, 0.0058347339, -0.0235630436, -0.0479795964, -0.0218184359, -0.0071361172, -0.0151966912
  , -0.0097784855, -0.0095681495, -0.0034165915, 0.3297981389 ]
dataL =
  [ -2.601732902, -1.316696363, -0.871222130, -0.654183201, -0.522267791, -0.443428053, -0.354395771, -0.333991149
  , -0.295183994, -0.296439580, -0.229544376, -0.222637642, -0.202047985, -0.204204804, -0.183726317, -0.136610008
  , -0.156998865, -0.141034029, -0.143865369, -0.129197535, -0.133972479, -0.126926659, -0.128347838, -0.157117220
  , -0.109735442, 0.033368215, -0.006847297, -0.107428176, -0.090519784, -0.084319856, -0.091435678, -0.116981475
  , -0.035683926, -0.055871286, -0.070714339, -0.171810133, -0.188127407, -0.173190332, -0.177037382, -0.150247961
  , -0.161869444, -0.159085298, 0.063624221, 0.155385493, 0.205595277, -0.062646700, -0.086759405, -0.116170913
  , -0.155975905, -0.160436464, -0.156902319, -0.078298756, -0.110387381, -0.108964450, -0.107384923, -0.112029816
  , -0.107684112, -0.109699141, -0.094061381, -0.090681046, -0.084130568, -0.088672375, -0.092348161, -0.093776304
  , -0.080702800, -0.074000132, -0.082043029, -0.075411382, -0.075062706, -0.066348414, -0.079317045, -0.084612389
  , -0.080266833, -0.062373969, -0.081955591, -0.074544338, -0.065770715, -0.061799502, -0.059769785, -0.058912678
  , -0.076710545, -0.059223647, -0.083529944, -0.108642253, -0.088163488, -0.080735023, -0.089995793, -0.089466481
  , -0.090513354, -0.090715129, 0.153778152 ]
dataU = 
  [ 0.294971776, 0.132408635, 0.095641357, 0.071712024, 0.059418880, 0.041921346, 0.062376117, 0.031286929, 0.030176255
  , -0.002982100, 0.037903312, 0.023972018, 0.025948935, 0.007786381, 0.013789841, 0.051403828, 0.020226641, 0.027341019
  , 0.016544568, 0.023246115, 0.011675392, 0.012972329, 0.005637162, -0.031167883, 0.014113206, 0.214070942, 0.110128057
  , 0.003266936, 0.016425030, 0.019331946, 0.009443626, -0.019659551, 0.061071251, 0.034616008, 0.015289927, -0.088627106
  , -0.107215797, -0.064381434, 0.100085727, 0.075114810, 0.088145348, 0.087188029, 0.245510159, 0.358399110, 0.381216282
  , 0.003734296, -0.022379107, -0.053478912, -0.064699509, -0.060256238, -0.057812964, -0.050199917, -0.013213131, 0.023038420
  , 0.001729044, 0.017328455, 0.054793397, 0.056055580, 0.042551761, 0.039688438, 0.034005001, 0.039426727, 0.043872557
  , 0.045999855, 0.033482417, 0.028026427, 0.032677113, 0.029491258, 0.029693607, 0.021408406, 0.035515983, 0.040979706
  , 0.037388630, 0.019491869, 0.038335749, 0.030609359, 0.022727062, 0.019654787, 0.019183234, 0.018640944,0.037029245
  , 0.019818145, 0.046280909, 0.011304464, 0.044856827, 0.045359973, 0.055832957, 0.055019139, 0.057073314, 0.056147911
  , 0.349947332 ]
-- minimum of dataL
dataBase = -2.61


