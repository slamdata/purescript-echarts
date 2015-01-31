module Utils where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import DOM
import Math (floor, round, pow)
import Data.Array
import Data.Maybe
import Data.Tuple
import qualified Data.DOM.Simple.Window as Win
import qualified Data.DOM.Simple.Document as Doc
import qualified Data.DOM.Simple.Element as El
import qualified Data.DOM.Simple.Events as Ev
import Data.DOM.Simple.Types (DOMEvent(..))

getElementById id = do
  doc <- Win.document Win.globalWindow
  El.getElementById id doc


onLoad action = 
  let actionWrap :: DOMEvent -> Eff _ Unit
      actionWrap _ = action
  in Ev.addUIEventListener Ev.LoadEvent actionWrap Win.globalWindow



precise :: Number -> Number -> Number
precise pre num =
  (round $  (pow 10 pre) * num) / (pow 10 pre)

-- sequence_ $ traverse (\i -> random) (1..10000) -> stackoverflow
foreign import randomLst """
function randomLst(count) {
  return function() {
    var Math = window['Math'];
    var result = [];
    for (var i = 0; i < count; i++) {
      result.push(Math.random());
    }
    return result;
  };
}
""" :: forall e.  Number -> Eff (random :: Random|e) [Number]


randomInList :: forall a e. [a] -> Eff (random :: Random|e) (Tuple a Number)
randomInList lst = do 
  let l = length lst
  rnd <- random
  let i = floor (rnd * l)
  return $ case lst !! i of
    Just x -> Tuple x i


      
