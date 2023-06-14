module Utils
  ( randomInArray
  , getElementById
  , randomArray
  , precise
  , onLoad
  ) where

import Prelude

-- import Control.Monad.Eff (Eff)
import Effect (Effect)
import Effect.Random (random)
-- import Control.Monad.Except (runExcept)

import Data.Array ((!!), length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
-- import Data.Either (either)
import Data.Tuple (Tuple(..))
-- import Data.Foreign (toForeign)
import Data.NonEmpty as NE

-- import DOM (DOM)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.HTML.Window (document, toEventTarget)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM.NonElementParentNode as NEPN
import Web.DOM.Document (toNonElementParentNode)
import DOM.Node.Types (ElementId)

import Data.Number (round, pow)
-- import Partial.Unsafe (unsafePartial)

-- toNonElementParentNode :: Document -> NonElementParentNode
-- getElementById :: String -> NonElementParentNode -> Effect (Maybe Element)
-- fromElement :: Element -> Maybe HTMLElement

-----------------------------------------------------------------------------
-- htmlDocumentToNonElementParentNode :: HTMLDocument -> NonElementParentNode
-- readHTMLElement :: Foreign -> F HTMLElement
-- windowToEventTarget :: Window -> EventTarget
-- windowToEventTarget = toEventTarget

getElementById
  ∷ ElementId
  → Effect (Maybe HTMLElement)
getElementById elementId = do
  win ← window
  doc ← document win
  el ← NEPN.getElementById elementId (toNonElementParentNode $ toDocument doc)
  pure $ join $ map fromElement el

onLoad
  ∷ ∀ a
  . Effect a
  → Effect Unit
onLoad handler = do
  el <- eventListener (const handler)
  addEventListener load el false
    <<< toEventTarget -- windowToEventTarget
    =<< window

precise ∷ Number → Number → Number
precise pre num =
  (round $ (pow 10.0 pre) * num) / (pow 10.0 pre)

foreign import randomArrayImpl
  ∷ ∀ a
  . (a → Array a → NE.NonEmpty Array a)
  → Int
  → Effect (NE.NonEmpty Array Number)

randomArray ∷ Int → Effect (NE.NonEmpty Array Number)
randomArray = randomArrayImpl NE.NonEmpty

randomInArray
  ∷ ∀ a
  . NE.NonEmpty Array a
  → Effect (Tuple a Int)
randomInArray nelst = do
  rnd ← random
  let
    lst = NE.oneOf nelst
    l = length lst
    i = Int.floor (rnd * Int.toNumber l)
  pure $ case lst !! i of
    Nothing → Tuple (NE.head nelst) 0
    Just x → Tuple x i
