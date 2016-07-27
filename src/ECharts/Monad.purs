module ECharts.Monad where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array as Arr
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple(..), uncurry, snd)
import Data.Foldable as F

import ECharts.Internal (unsafeSetField, emptyObject)

newtype DSL (i ∷ # !) a = DSL (Writer (Array (Tuple String Foreign)) a)
unDSL ∷ ∀ i a. DSL i a → Writer (Array (Tuple String Foreign)) a
unDSL (DSL cs) = cs

instance functorDSL ∷ Functor (DSL i) where
  map f (DSL o) = DSL $ map f o

instance applyDSL ∷ Apply (DSL i) where
  apply (DSL f) (DSL o) = DSL $ apply f o

instance applicativeDSL ∷ Applicative (DSL i) where
  pure = DSL <<< pure

instance bindDSL ∷ Bind (DSL i) where
  bind (DSL o) f = DSL $ o >>= unDSL <<< f

instance monadDSL ∷ Monad (DSL i)

set ∷ ∀ i. String → Foreign → DSL i Unit
set k v = DSL $ tell $ Arr.singleton $ Tuple k v

applyOnePair ∷ Tuple String Foreign → Foreign → Foreign
applyOnePair opt obj = uncurry (unsafeSetField obj) opt

buildObj ∷ ∀ i. DSL i Unit → Foreign
buildObj (DSL cs) =
  F.foldr applyOnePair (emptyObject unit) $ execWriter cs

buildSeries ∷ ∀ i. DSL i Unit → Foreign
buildSeries (DSL cs) =
  toForeign $ map (\(Tuple ty f) → unsafeSetField f "type" $ toForeign ty) $ execWriter cs

buildArr ∷ ∀ i. DSL i Unit → Foreign
buildArr (DSL cs) =
  toForeign $ map snd $ execWriter cs
