module ECharts.Loading where

import ECharts.Common
import ECharts.Coords
import ECharts.Chart
import ECharts.Style.Text

import Data.Array (concat)
import Data.Function
import Data.Maybe
import Data.Tuple.Nested
import qualified Data.StrMap as M
import Control.Monad.Eff

import Data.Argonaut.Core
import Data.Argonaut.Combinators
import Data.Argonaut.Encode

data LoadingEffect = Spin | Bar | Ring | Whirling | DynamicLine | Bubble

instance loadingEffectShot :: Show LoadingEffect where
  show eff = case eff of
    Spin -> "spin"
    Bar -> "bar"
    Ring -> "ring"
    Whirling -> "whirling"
    DynamicLine -> "dynamicLine"
    Bubble -> "bubble"

instance loadingEffectEq :: Eq LoadingEffect where
  (==) a b = (show a) == (show b)
  (/=) a b = not $ a == b

instance loadingEffectEncodeJson :: EncodeJson LoadingEffect where
  encodeJson = fromString <<< show 


newtype LoadingOption = 
  LoadingOption {
    text :: Maybe String,
    x :: Maybe XPos,
    y :: Maybe YPos,
    textStyle :: Maybe TextStyle,
    effect :: Maybe LoadingEffect,
    effectOption :: Maybe Json,
    progress :: Maybe Number
    }

instance showLoadingOptions :: EncodeJson LoadingOption where
  encodeJson (LoadingOption options) =
    fromObject $ M.fromList [
      "text" := options.text,
      "x" := options.x,
      "y" := options.y,
      "textStyle" := options.textStyle,
      "effect" := options.effect,
      "effectOption" := options.effectOption,
      "progress" := options.progress
      ]


foreign import showLoadingImpl """
function showLoadingImpl(json, chart) {
  return function() {
    return chart.showLoading(json);
  };
}
""" :: forall e a. Fn2 Json EChart (Eff e EChart)

showLoading :: forall e. LoadingOption -> EChart -> Eff e EChart
showLoading opts chart =
  runFn2 showLoadingImpl (encodeJson opts) chart




foreign import hideLoading """
function hideLoading(chart) {
  return function() {
    return chart.hideLoading();
  };
}
""" :: forall e. EChart -> Eff e EChart


nullOption =
  {
    text: Nothing,
    x: Nothing,
    y: Nothing,
    textStyle: Nothing,
    effect: Nothing,
    effectOption: Nothing,
    progress: Nothing
  }
