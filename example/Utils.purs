module Utils where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Bind ((=<<), (<=<))

import Data.Array
import Data.Int (fromNumber, toNumber)
import Data.Maybe
import Data.Either (either)
import Data.Tuple
import Data.Nullable (toMaybe)
import Data.Foreign (toForeign)

import DOM (DOM())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.Event.Types (Event())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement(), windowToEventTarget, htmlDocumentToNonElementParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element(), ElementId())
import qualified DOM.Node.NonElementParentNode as NEPN

import Math (floor, round, pow)

getElementById :: forall eff. ElementId -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
getElementById elementId = do
  win <- window
  doc <- document win
  el <- NEPN.getElementById elementId (htmlDocumentToNonElementParentNode doc)
  pure $ either (const Nothing) Just $ readHTMLElement (toForeign el)

onLoad :: forall eff a. Eff (dom :: DOM | eff) a -> Eff (dom :: DOM | eff) Unit
onLoad handler = addEventListener load (eventListener (const handler)) false <<< windowToEventTarget =<< window

precise :: Number -> Number -> Number
precise pre num =
  (round $ (pow 10.0 pre) * num) / (pow 10.0 pre)

-- sequence_ $ traverse (\i -> random) (1..10000) -> stackoverflow
foreign import randomLst :: forall e.  Number -> Eff (random :: RANDOM|e) (Array Number)

randomInList :: forall a e. Array a -> Eff (random :: RANDOM|e) (Tuple a Number)
randomInList lst = do
  let l = length lst
  rnd <- random
  let i = floor (rnd * toNumber l)
  return $ case fromNumber i >>= (lst !!) of
    Just x -> Tuple x i
