module ECharts.Option where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Color as C

import Data.Array as Arr
import Data.Foldable as F

import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Tooltip as ET
import ECharts.Grid as EG
import ECharts.Legend as EL
import ECharts.XAxis as EX
import ECharts.YAxis as EY
import ECharts.Internal (unsafeSetField, emptyObject)

data OptionC
  = Tooltip T.Tooltip
  | Grid T.Grid
  | Legend T.Legend
  | XAxis T.XAxis
  | YAxis T.YAxis
  | Color (Array C.Color)
  | Series T.Series

newtype OptionM a = OptionM (Writer (Array OptionC) a)

instance functorOptionM ∷ Functor OptionM where
  map f (OptionM o) = OptionM $ map f o

instance applyOptionM ∷ Apply OptionM where
  apply (OptionM f) (OptionM o) = OptionM $ apply f o

instance applicativeOptionM ∷ Applicative OptionM where
  pure = OptionM <<< pure

instance bindOptionM ∷ Bind OptionM where
  bind (OptionM o) f = OptionM $ o >>= (\(OptionM o') → o') <<< f

instance monadOptionM ∷ Monad OptionM

-- F for Foreign

tooltipF ∷ T.Tooltip → OptionM Unit
tooltipF = OptionM <<< tell <<< Arr.singleton <<< Tooltip

tooltip ∷ ET.TooltipM Unit → OptionM Unit
tooltip = tooltipF <<< ET.buildTooltip

gridF ∷ T.Grid → OptionM Unit
gridF = OptionM <<< tell <<< Arr.singleton <<< Grid

grid ∷ EG.GridM Unit → OptionM Unit
grid = gridF <<< EG.buildGrid

legendF ∷ T.Legend → OptionM Unit
legendF = OptionM <<< tell <<< Arr.singleton <<< Legend

legend ∷ EL.LegendM Unit → OptionM Unit
legend = legendF <<< EL.buildLegend

xAxisF ∷ T.XAxis → OptionM Unit
xAxisF = OptionM <<< tell <<< Arr.singleton <<< XAxis

xAxis ∷ EX.XAxisM Unit → OptionM Unit
xAxis = xAxisF <<< EX.buildXAxis

yAxisF ∷ T.YAxis → OptionM Unit
yAxisF = OptionM <<< tell <<< Arr.singleton <<< YAxis

yAxis ∷ EY.YAxisM Unit → OptionM Unit
yAxis = yAxisF <<< EY.buildYAxis

color ∷ Array C.Color → OptionM Unit
color = OptionM <<< tell <<< Arr.singleton <<< Color


optionTuple ∷ OptionC → Tuple String Foreign
optionTuple = case _ of
  Tooltip f → Tuple "tooltip" $ T.unTooltip f
  Grid f → Tuple "grid" $ T.unGrid f
  Legend f → Tuple "legend" $ T.unLegend f
  XAxis f → Tuple "xAxis" $ T.unXAxis f
  YAxis f → Tuple "yAxis" $ T.unYAxis f

  Color f → Tuple "color" $ toForeign $ map C.toHexString f
  Series f → Tuple "series" $ T.unSeries f

buildOption ∷ OptionM Unit → T.Option
buildOption (OptionM cs) =
  let
    commands ∷ Array OptionC
    commands = execWriter cs

    foldFn ∷ OptionC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (optionTuple opt)
  in
    T.Option $ F.foldr foldFn (emptyObject unit) commands
