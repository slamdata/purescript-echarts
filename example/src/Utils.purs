module Utils
  ( randomInArray
  , getElementById
  , randomArray
  , precise
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Bind ((=<<), (<=<))

import Data.Array ((!!), length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Data.Foreign (toForeign)
import Data.NonEmpty as NE

import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML.Types (HTMLElement, windowToEventTarget, htmlDocumentToNonElementParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode as NEPN
import DOM.Node.Types (ElementId)

import Math (round, pow)

getElementById
  ∷ ∀ eff
  . ElementId
  → Eff (dom ∷ DOM | eff) (Maybe HTMLElement)
getElementById elementId = do
  win ← window
  doc ← document win
  el ← NEPN.getElementById elementId (htmlDocumentToNonElementParentNode doc)
  pure $ either (const Nothing) Just $ readHTMLElement (toForeign el)

onLoad
  ∷ ∀ eff a
  . Eff (dom ∷ DOM | eff) a
  → Eff (dom ∷ DOM | eff) Unit
onLoad handler =
  addEventListener load (eventListener (const handler)) false
    <<< windowToEventTarget
    =<< window

precise ∷ Number → Number → Number
precise pre num =
  (round $ (pow 10.0 pre) * num) / (pow 10.0 pre)

foreign import randomArrayImpl
  ∷ ∀ e a
  . (a → Array a → NE.NonEmpty Array a)
  → Int
  → Eff (random ∷ RANDOM|e) (NE.NonEmpty Array Number)

randomArray ∷ ∀ e. Int → Eff (random ∷ RANDOM|e) (NE.NonEmpty Array Number)
randomArray = randomArrayImpl NE.NonEmpty

randomInArray
  ∷ ∀ a e
  . NE.NonEmpty Array a
  → Eff (random ∷ RANDOM|e) (Tuple a Int)
randomInArray nelst = do
  rnd ← random
  let
    lst = NE.oneOf nelst
    l = length lst
    i = Int.floor (rnd * Int.toNumber l)
  pure $ case lst !! i of
    Nothing → Tuple (NE.head nelst) 0
    Just x → Tuple x i
