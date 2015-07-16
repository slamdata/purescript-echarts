module Utils where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import DOM
import Math (floor, round, pow)
import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Int (fromNumber, toNumber)
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
  (round $  (pow 10.0 pre) * num) / (pow 10.0 pre)

-- sequence_ $ traverse (\i -> random) (1..10000) -> stackoverflow
foreign import randomLst :: forall e.  Number -> Eff (random :: RANDOM|e) (Array Number)


randomInList :: forall a e. Array a -> Eff (random :: RANDOM|e) (Tuple a Number)
randomInList lst = do 
  let l = length lst
  rnd <- random
  let i = floor (rnd * toNumber l)
  return $ case fromNumber i >>= (lst !!) of
    Just x -> Tuple x i


      
