module ECharts.Chart (
  EChart(),
  ZRender(),
  Theme(..),
  init,
  setTheme,
  getZRender,
  resize,
  refresh,
  clear,
  dispose
  ) where

import DOM
import Data.Maybe 
import Data.Function
import Data.DOM.Simple.Types (HTMLElement())
import Control.Monad.Eff

import Data.Argonaut.Core
import Data.Argonaut.Encode

import ECharts.Effects



foreign import data EChart :: *
foreign import data ZRender :: *

data Theme = ThemeName String | ThemeConfig Json
instance themeEncodeJson :: EncodeJson Theme where
  encodeJson theme = case theme of
    ThemeName name -> fromString name
    ThemeConfig a -> encodeJson a

foreign import initImpl """
function initImpl(node, theme) {
  return function() {
    return echarts.init(node, theme);
  };
}
""" :: forall e. Fn2 HTMLElement Json (Eff (dom :: DOM, echartInit :: ECHARTS_INIT|e) EChart)

init :: forall e.  Maybe Theme -> HTMLElement ->
        Eff (dom :: DOM, echartInit :: ECHARTS_INIT|e) EChart
init theme dom = 
  runFn2 initImpl dom (encodeJson theme)


foreign import setThemeImpl """
function setThemeImpl(args, chart) {
  return function() {
    chart.setTheme.apply(chart, args);
  };
}
""" :: forall e. Fn2 Json EChart (Eff e EChart)

setTheme :: forall e. Theme -> EChart ->
            Eff (dom :: DOM, echartTheme :: ECHARTS_THEME_SET|e) EChart
setTheme theme chart = do
  runFn2 setThemeImpl (encodeJson theme) chart


foreign import getZRender """
function getZRender(chart) {
  return function() {
    return chart.getZRender();
  };
}
""" :: forall e. EChart -> Eff e ZRender


foreign import resize """
function resize(chart) {
  return function() {
    return chart.resize();
  };
}
""" :: forall e. EChart -> Eff (dom :: DOM, echartResize :: ECHARTS_RESIZE|e) Unit


foreign import refresh """
function refresh(chart) {
  return function( ){
    return chart.refresh();
  };
}
""" :: forall e. EChart -> Eff (dom :: DOM, echartRefresh :: ECHARTS_REFRESH|e) Unit


foreign import clear """
function clear(chart) {
  return function() {
    return chart.clear();
  };
}
""" :: forall e. EChart -> Eff (dom :: DOM, echartClear :: ECHARTS_CLEAR|e) Unit


foreign import dispose """
function dispose() {
  return function() {
    return chart.dispose();
  };
}
""" :: forall e. EChart -> Eff (dom :: DOM, echartDispose :: ECHARTS_DISPOSE|e) Unit
