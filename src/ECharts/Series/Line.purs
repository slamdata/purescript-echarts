module ECharts.Series.Line where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry)

import ECharts.Types as T
import ECharts.Internal (unsafeSetField, emptyObject)

data LineC
  = Name String
  | XAxisIndex Int
  | YAxisIndex Int
  | PolarIndex Int
  | Symbol T.Symbol
  | SymbolSize T.SymbolSize
  | LineStyle T.LineStyle
  | ItemStyle T.ItemStyle
  | AreaStyle T.AreaStyle
  | Smooth Boolean
  | Items (Array T.Item)

newtype LineM a = LineM (Writer (Array LineC) a)

instance functorLineM ∷ Functor LineM where
  map f (LineM o) = LineM $ map f o

instance applyLineM ∷ Apply LineM where
  apply (LineM f) (LineM o) = LineM $ apply f o

instance applicativeLineM ∷ Applicative LineM where
  pure = LineM <<< pure

instance bindLineM ∷ Bind LineM where
  bind (LineM o) f = LineM $ o >>= (\(LineM o') → o') <<< f

instance monadLineM ∷ Monad LineM

name ∷ String → LineM Unit
name = LineM <<< tell <<< Arr.singleton <<< Name

xAxisIndex ∷ Int → LineM Unit
xAxisIndex = LineM <<< tell <<< Arr.singleton <<< XAxisIndex

yAxisIndex ∷ Int → LineM Unit
yAxisIndex = LineM <<< tell <<< Arr.singleton <<< YAxisIndex

polarIndex ∷ Int → LineM Unit
polarIndex = LineM <<< tell <<< Arr.singleton <<< PolarIndex

symbol ∷ T.Symbol → LineM Unit
symbol = LineM <<< tell <<< Arr.singleton <<< Symbol

symbolSize ∷ T.SymbolSize → LineM Unit
symbolSize = LineM <<< tell <<< Arr.singleton <<< SymbolSize

lineStyle ∷ T.LineStyle → LineM Unit
lineStyle = LineM <<< tell <<< Arr.singleton <<< LineStyle

itemStyle ∷ T.ItemStyle → LineM Unit
itemStyle = LineM <<< tell <<< Arr.singleton <<< ItemStyle

areaStyle ∷ T.AreaStyle → LineM Unit
areaStyle = LineM <<< tell <<< Arr.singleton <<< AreaStyle

smooth ∷ Boolean → LineM Unit
smooth = LineM <<< tell <<< Arr.singleton <<< Smooth

items ∷ Array T.Item → LineM Unit
items = LineM <<< tell <<< Arr.singleton <<< Items

lineTuple ∷ LineC → Tuple String Foreign
lineTuple = case _ of
  Name s → Tuple "name" $ toForeign s
  XAxisIndex i → Tuple "xAxisIndex" $ toForeign i
  YAxisIndex i → Tuple "yAxisIndex" $ toForeign i
  PolarIndex i → Tuple "polarIndex" $ toForeign i
  Symbol s → Tuple "symbol" $ toForeign $ T.printSymbol s
  SymbolSize s → Tuple "symbolSize" $ T.unSymbolSize s
  LineStyle s → Tuple "lineStyle" $ T.unLineStyle s
  AreaStyle s → Tuple "areaStyle" $ T.unAreaStyle s
  ItemStyle s → Tuple "itemStyle" $ T.unItemStyle s
  Smooth b → Tuple "smooth" $ toForeign b
  Items i → Tuple "data" $ toForeign i

buildLine ∷ LineM Unit → T.LineSeries
buildLine (LineM cs) =
  let
    foldFn ∷ LineC → Foreign → Foreign
    foldFn opt obj = uncurry (unsafeSetField obj) (lineTuple opt)
  in
    T.LineSeries $ F.foldr foldFn (emptyObject unit) $ execWriter cs
