module ECharts.Monad where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, discard, flip, map, pure, unit, (#), ($), (<<<), (>>>))

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Effect.Class (class MonadEffect)
-- import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Cont.Class (class MonadCont)
-- import Control.Monad.Eff (kind Effect)
-- import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell)
import Control.Monad.Writer.Trans (WriterT, execWriterT, runWriterT)
import Control.MonadPlus (class MonadPlus)
-- import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus, empty)
import Data.Array as Arr
import Data.Bifunctor (rmap)
import Data.Foldable as F
import Data.Foreign (Foreign, toForeign)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), uncurry, snd) --, lookup)
import ECharts.Internal (unsafeSetField, emptyObject)
import ECharts.Types as ET

type KeyVal = Tuple String Foreign
type Pairs = Array KeyVal

-- newtype CommandsT (i ∷ # Effect) m a = CommandsT (WriterT Pairs m a)
newtype CommandsT :: (Type -> Type) -> Type -> Type
newtype CommandsT m a = CommandsT (WriterT Pairs m a)



type DSL :: (Type -> Type) -> Type
type DSL m = CommandsT m Unit
type DSL' :: Type
type DSL' = CommandsT Identity Unit

derive instance newtypeCommandsT
  ∷ Newtype (CommandsT m a) _
derive newtype instance functorDSL
  ∷ Functor m ⇒ Functor (CommandsT m)
derive newtype instance applyDSL
  ∷ Apply m ⇒ Apply (CommandsT m)
derive newtype instance applicativeDSL
  ∷ Applicative m ⇒ Applicative (CommandsT m)
derive newtype instance bindDSL
  ∷ Bind m ⇒ Bind (CommandsT m)
derive newtype instance monadDSL
  ∷ Monad m ⇒ Monad (CommandsT m)
derive newtype instance monadTellCommandsT
  ∷ Monad m ⇒ MonadTell (Array (Tuple String Foreign)) (CommandsT m)
derive newtype instance monadWriterCommandsT
  ∷ Monad m ⇒ MonadWriter (Array (Tuple String Foreign)) (CommandsT m)
derive newtype instance plusCommandsT
  ∷ Plus m ⇒ Plus (CommandsT m)
derive newtype instance altCommandsT
  ∷ Alt m ⇒ Alt (CommandsT m)
derive newtype instance alternativeCommandsT
  ∷ Alternative m ⇒ Alternative (CommandsT m)
derive newtype instance monadRecCommandsT
  ∷ MonadRec m ⇒ MonadRec (CommandsT m)
-- derive newtype instance monadZeroCommandsT
--   ∷ MonadZero m ⇒ MonadZero (CommandsT m)
derive newtype instance monadPlusCommandsT
  ∷ MonadPlus m ⇒ MonadPlus (CommandsT m)
derive newtype instance monadAffCommandsT
  ∷ MonadEffect m ⇒ MonadEffect (CommandsT m)
-- derive newtype instance monadEffCommandsT
--   ∷ MonadEff e m ⇒ MonadEff e (CommandsT m)
derive newtype instance monadContCommandsT
  ∷ MonadCont m ⇒ MonadCont (CommandsT m)
derive newtype instance monadThrowCommandsT
  ∷ MonadThrow e m ⇒ MonadThrow e (CommandsT m)
derive newtype instance monadErrorCommandsT
  ∷ MonadError e m ⇒ MonadError e (CommandsT m)
derive newtype instance monadAskCommandsT
  ∷ MonadAsk r m ⇒ MonadAsk r (CommandsT m)
derive newtype instance monadReaderCommandsT
  ∷ MonadReader r m ⇒ MonadReader r (CommandsT m)
derive newtype instance monadStateCommandsT
  ∷ MonadState s m ⇒ MonadState s (CommandsT m)

derive newtype instance monadTransCommandsT ∷ MonadTrans (CommandsT)

set' ∷ ∀ m. MonadTell Pairs m ⇒ String → Foreign → m Unit
set' k v = tell $ Arr.singleton $ Tuple k v

set ∷ ∀ m a. MonadTell Pairs m ⇒ String → Tuple a Foreign → m a
set k (Tuple a v) = do
  tell $ Arr.singleton $ Tuple k v
  pure a

get
  ∷ ∀ m f a
  . Monad m
  ⇒ Alternative f
  ⇒ String
  → CommandsT m a
  → CommandsT m (Tuple a (f Foreign))
get k cs =
  lift
    $ unwrap cs
    # runWriterT
    >>> map (rmap (maybe empty pure <<< F.lookup k))

lastWithKeys
  ∷ ∀ f m w a
  . F.Foldable f
  ⇒ Monad m
  ⇒ Alternative w
  ⇒ f String
  → CommandsT m a
  → CommandsT m (Tuple a (w Foreign))
lastWithKeys ks cs =
  lift
    $ unwrap cs
    # runWriterT
    >>> map (rmap (maybe empty pure <<< F.foldl (lookup' ks) Nothing))
  where
  lookup' ∷ f String → Maybe Foreign → Tuple String Foreign → Maybe Foreign
  lookup' ks' Nothing (Tuple kk f) | F.elem kk ks' = Just f
  lookup' _ a _ = a

applyOnePair ∷ Tuple String Foreign → Foreign → Foreign
applyOnePair opt obj = uncurry (unsafeSetField obj) opt

interpretT ∷ ∀ m a. Functor m ⇒ CommandsT m a → m ET.Option
interpretT cs =
  map ET.Option $ unwrap cs # execWriterT >>> map (F.foldl (flip applyOnePair) $ emptyObject unit)

interpret ∷ DSL' → ET.Option
interpret = unwrap <<< interpretT

buildObj ∷ ∀ m a. Monad m ⇒ CommandsT m a → CommandsT  m (Tuple a Foreign)
buildObj cs =
  lift $ unwrap cs # runWriterT >>> map (rmap $ F.foldl (flip applyOnePair) $ emptyObject unit)

buildSeries ∷ ∀ m a. Monad m ⇒ CommandsT m a → CommandsT m (Tuple a Foreign)
buildSeries cs =
  lift $ unwrap cs # runWriterT >>> map (rmap $ toForeign <<< typify)
  where
  typify = map \(Tuple ty f) → unsafeSetField f "type" $ toForeign ty

buildArr ∷ ∀ m a. Monad m ⇒ CommandsT m a → CommandsT m (Tuple a Foreign)
buildArr cs =
  lift $ unwrap cs # runWriterT >>> map (rmap $ toForeign <<< map snd)
