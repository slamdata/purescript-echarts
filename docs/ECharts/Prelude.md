## Module ECharts.Prelude


### Re-exported from Control.Alt:

#### `(<|>)`

``` purescript
infixl 3 alt as <|>
```

### Re-exported from Control.Monad.Eff:

#### `Eff`

``` purescript
data Eff :: # ! -> * -> *
```

The `Eff` type constructor is used to represent _native_ effects.

See [Handling Native Effects with the Eff Monad](http://www.purescript.org/learn/eff/)
for more details.

The first type parameter is a row of effects which represents the contexts
in which a computation can be run, and the second type parameter is the
return type.

##### Instances
``` purescript
Functor (Eff e)
Apply (Eff e)
Applicative (Eff e)
Bind (Eff e)
Monad (Eff e)
```

### Re-exported from Data.Argonaut:

#### `Json`

``` purescript
data Json :: *
```

The type of JSON data. The underlying representation is the same as what
would be returned from JavaScript's `JSON.stringify` function; that is,
ordinary JavaScript booleans, strings, arrays, objects, etc.

##### Instances
``` purescript
Eq Json
Ord Json
Show Json
```

#### `JObject`

``` purescript
type JObject = StrMap Json
```

A JSON object; a JavaScript object containing `Json` values.

#### `JAssoc`

``` purescript
type JAssoc = Tuple String Json
```

#### `DecodeJson`

``` purescript
class DecodeJson a where
  decodeJson :: Json -> Either String a
```

##### Instances
``` purescript
(DecodeJson a) => DecodeJson (Maybe a)
(DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b)
(DecodeJson a, DecodeJson b) => DecodeJson (Either a b)
DecodeJson Unit
DecodeJson Boolean
DecodeJson Number
DecodeJson Int
DecodeJson String
DecodeJson Json
DecodeJson Char
(DecodeJson a) => DecodeJson (StrMap a)
(DecodeJson a) => DecodeJson (Array a)
(DecodeJson a) => DecodeJson (List a)
(Ord a, DecodeJson a, DecodeJson b) => DecodeJson (Map a b)
```

#### `EncodeJson`

``` purescript
class EncodeJson a where
  encodeJson :: a -> Json
```

##### Instances
``` purescript
(EncodeJson a) => EncodeJson (Maybe a)
(EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b)
(EncodeJson a, EncodeJson b) => EncodeJson (Either a b)
EncodeJson Unit
EncodeJson Boolean
EncodeJson Number
EncodeJson Int
EncodeJson String
EncodeJson Json
EncodeJson Char
(EncodeJson a) => EncodeJson (Array a)
(EncodeJson a) => EncodeJson (List a)
(EncodeJson a) => EncodeJson (StrMap a)
(Ord a, EncodeJson a, EncodeJson b) => EncodeJson (Map a b)
```

#### `(:=)`

``` purescript
infix 7 Data.Argonaut.Encode.Combinators.assoc as :=
```

Creates a `JAssoc` entry, representing a key/value pair for an object.

#### `(.?)`

``` purescript
infix 7 Data.Argonaut.Decode.Combinators.getField as .?
```

### Re-exported from Data.Either:

#### `Either`

``` purescript
data Either a b
  = Left a
  | Right b
```

The `Either` type is used to represent a choice between two types of value.

A common use case for `Either` is error handling, where `Left` is used to
carry an error value and `Right` is used to carry a success value.

##### Instances
``` purescript
Functor (Either a)
Invariant (Either a)
Bifunctor Either
Apply (Either e)
Applicative (Either e)
Alt (Either e)
Bind (Either e)
Monad (Either e)
Extend (Either e)
(Show a, Show b) => Show (Either a b)
(Eq a, Eq b) => Eq (Either a b)
(Ord a, Ord b) => Ord (Either a b)
(Bounded a, Bounded b) => Bounded (Either a b)
Foldable (Either a)
Bifoldable Either
Traversable (Either a)
Bitraversable Either
(Semiring b) => Semiring (Either a b)
(Semigroup b) => Semigroup (Either a b)
```

### Re-exported from Data.Maybe:

#### `Maybe`

``` purescript
data Maybe a
  = Just a
  | Nothing
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.

##### Instances
``` purescript
Functor Maybe
Apply Maybe
Applicative Maybe
Alt Maybe
Plus Maybe
Alternative Maybe
Bind Maybe
Monad Maybe
MonadZero Maybe
Extend Maybe
Invariant Maybe
(Semigroup a) => Semigroup (Maybe a)
(Semigroup a) => Monoid (Maybe a)
(Eq a) => Eq (Maybe a)
(Ord a) => Ord (Maybe a)
(Bounded a) => Bounded (Maybe a)
(Show a) => Show (Maybe a)
```

#### `maybe`

``` purescript
maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
```

Takes a default value, a function, and a `Maybe` value. If the `Maybe`
value is `Nothing` the default value is returned, otherwise the function
is applied to the value inside the `Just` and the result is returned.

``` purescript
maybe x f Nothing == x
maybe x f (Just y) == f y
```

### Re-exported from Data.Tuple:

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

##### Instances
``` purescript
(Show a, Show b) => Show (Tuple a b)
(Eq a, Eq b) => Eq (Tuple a b)
(Ord a, Ord b) => Ord (Tuple a b)
(Bounded a, Bounded b) => Bounded (Tuple a b)
Semigroupoid Tuple
(Semigroup a, Semigroup b) => Semigroup (Tuple a b)
(Monoid a, Monoid b) => Monoid (Tuple a b)
(Semiring a, Semiring b) => Semiring (Tuple a b)
(Ring a, Ring b) => Ring (Tuple a b)
(CommutativeRing a, CommutativeRing b) => CommutativeRing (Tuple a b)
(HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (Tuple a b)
(BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)
Functor (Tuple a)
Invariant (Tuple a)
Bifunctor Tuple
(Semigroup a) => Apply (Tuple a)
Biapply Tuple
(Monoid a) => Applicative (Tuple a)
Biapplicative Tuple
(Semigroup a) => Bind (Tuple a)
(Monoid a) => Monad (Tuple a)
Extend (Tuple a)
Comonad (Tuple a)
(Lazy a, Lazy b) => Lazy (Tuple a b)
Foldable (Tuple a)
Bifoldable Tuple
Traversable (Tuple a)
Bitraversable Tuple
```

### Re-exported from Prelude:

#### `Void`

``` purescript
newtype Void
```

##### Instances
``` purescript
Show Void
```

#### `Unit`

``` purescript
data Unit :: *
```

The `Unit` type has a single inhabitant, called `unit`. It represents
values with no computational content.

`Unit` is often used, wrapped in a monadic type constructor, as the
return type of a computation where only
the _effects_ are important.

##### Instances
``` purescript
Show Unit
```

#### `Ordering`

``` purescript
data Ordering
  = LT
  | GT
  | EQ
```

The `Ordering` data type represents the three possible outcomes of
comparing two values:

`LT` - The first value is _less than_ the second.
`GT` - The first value is _greater than_ the second.
`EQ` - The first value is _equal to_ the second.

##### Instances
``` purescript
Eq Ordering
Semigroup Ordering
Show Ordering
```

#### `Applicative`

``` purescript
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

The `Applicative` type class extends the [`Apply`](#apply) type class
with a `pure` function, which can be used to create values of type `f a`
from values of type `a`.

Where [`Apply`](#apply) provides the ability to lift functions of two or
more arguments to functions whose arguments are wrapped using `f`, and
[`Functor`](#functor) provides the ability to lift functions of one
argument, `pure` can be seen as the function which lifts functions of
_zero_ arguments. That is, `Applicative` functors support a lifting
operation for any number of function arguments.

Instances must satisfy the following laws in addition to the `Apply`
laws:

- Identity: `(pure id) <*> v = v`
- Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`

##### Instances
``` purescript
Applicative (Function r)
Applicative Array
```

#### `Apply`

``` purescript
class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
```

The `Apply` class provides the `(<*>)` which is used to apply a function
to an argument under a type constructor.

`Apply` can be used to lift functions of two or more arguments to work on
values wrapped with the type constructor `f`. It might also be understood
in terms of the `lift2` function:

```purescript
lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
```

`(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
the function application operator `($)` to arguments wrapped with the
type constructor `f`.

Instances must satisfy the following law in addition to the `Functor`
laws:

- Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`

Formally, `Apply` represents a strong lax semi-monoidal endofunctor.

##### Instances
``` purescript
Apply (Function r)
Apply Array
```

#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
```

The `Bind` type class extends the [`Apply`](#apply) type class with a
"bind" operation `(>>=)` which composes computations in sequence, using
the return value of one computation to determine the next computation.

The `>>=` operator can also be expressed using `do` notation, as follows:

```purescript
x >>= f = do y <- x
             f y
```

where the function argument of `f` is given the name `y`.

Instances must satisfy the following law in addition to the `Apply`
laws:

- Associativity: `(x >>= f) >>= g = x >>= (\k -> f k >>= g)`

Associativity tells us that we can regroup operations which use `do`
notation so that we can unambiguously write, for example:

```purescript
do x <- m1
   y <- m2 x
   m3 x y
```

##### Instances
``` purescript
Bind (Function r)
Bind Array
```

#### `BooleanAlgebra`

``` purescript
class (HeytingAlgebra a) <= BooleanAlgebra a
```

The `BooleanAlgebra` type class represents types that behave like boolean
values.

Instances should satisfy the following laws in addition to the
`HeytingAlgebra` law:

- Excluded middle:
  - `a || not a = tt`

##### Instances
``` purescript
BooleanAlgebra Boolean
BooleanAlgebra Unit
```

#### `Bounded`

``` purescript
class (Ord a) <= Bounded a where
  top :: a
  bottom :: a
```

The `Bounded` type class represents totally ordered types that have an
upper and lower boundary.

Instances should satisfy the following law in addition to the `Ord` laws:

- Bounded: `bottom <= a <= top`

##### Instances
``` purescript
Bounded Boolean
Bounded Int
Bounded Char
Bounded Ordering
Bounded Unit
```

#### `Category`

``` purescript
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

`Category`s consist of objects and composable morphisms between them, and
as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
must have an identity element.

Instances must satisfy the following law in addition to the
`Semigroupoid` law:

- Identity: `id <<< p = p <<< id = p`

##### Instances
``` purescript
Category Function
```

#### `CommutativeRing`

``` purescript
class (Ring a) <= CommutativeRing a
```

The `CommutativeRing` class is for rings where multiplication is
commutative.

Instances must satisfy the following law in addition to the `Ring`
laws:

- Commutative multiplication: `a * b = b * a`

##### Instances
``` purescript
CommutativeRing Int
CommutativeRing Number
CommutativeRing Unit
```

#### `Eq`

``` purescript
class Eq a where
  eq :: a -> a -> Boolean
```

The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`

##### Instances
``` purescript
Eq Boolean
Eq Int
Eq Number
Eq Char
Eq String
Eq Unit
Eq Void
(Eq a) => Eq (Array a)
```

#### `EuclideanRing`

``` purescript
class (CommutativeRing a) <= EuclideanRing a where
  degree :: a -> Int
  div :: a -> a -> a
  mod :: a -> a -> a
```

The `EuclideanRing` class is for commutative rings that support division.

Instances must satisfy the following law in addition to the `Ring`
laws:

- Integral domain: `a /= 0` and `b /= 0` implies `a * b /= 0`
- Multiplicative Euclidean function: ``a = (a / b) * b + (a `mod` b)``
  where `degree a > 0` and `degree a <= degree (a * b)`

##### Instances
``` purescript
EuclideanRing Int
EuclideanRing Number
EuclideanRing Unit
```

#### `Field`

``` purescript
class (EuclideanRing a) <= Field a
```

The `Field` class is for types that are commutative fields.

Instances must satisfy the following law in addition to the
`EuclideanRing` laws:

- Non-zero multiplicative inverse: ``a `mod` b = 0` for all `a` and `b`

##### Instances
``` purescript
Field Number
Field Unit
```

#### `Functor`

``` purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

A `Functor` is a type constructor which supports a mapping operation
`(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions
`f a -> f b` whose argument and return types use the type constructor `f`
to represent some computational context.

Instances must satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

##### Instances
``` purescript
Functor (Function r)
Functor Array
```

#### `HeytingAlgebra`

``` purescript
class HeytingAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a
```

The `HeytingAlgebra` type class represents types are bounded lattices with
an implication operator such that the following laws hold:

- Associativity:
  - `a || (b || c) = (a || b) || c`
  - `a && (b && c) = (a && b) && c`
- Commutativity:
  - `a || b = b || a`
  - `a && b = b && a`
- Absorption:
  - `a || (a && b) = a`
  - `a && (a || b) = a`
- Idempotent:
  - `a || a = a`
  - `a && a = a`
- Identity:
  - `a || ff = a`
  - `a && tt = a`
- Implication:
  - ``a `implies` a = tt``
  - ``a && (a `implies` b) = a && b``
  - ``b && (a `implies` b) = b``
  - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
- Complemented:
  - ``not a = a `implies` ff``

##### Instances
``` purescript
HeytingAlgebra Boolean
HeytingAlgebra Unit
(HeytingAlgebra b) => HeytingAlgebra (a -> b)
```

#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m
```

The `Monad` type class combines the operations of the `Bind` and
`Applicative` type classes. Therefore, `Monad` instances represent type
constructors which support sequential composition, and also lifting of
functions of arbitrary arity.

Instances must satisfy the following laws in addition to the
`Applicative` and `Bind` laws:

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`

##### Instances
``` purescript
Monad (Function r)
Monad Array
```

#### `Ord`

``` purescript
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering
```

The `Ord` type class represents types which support comparisons with a
_total order_.

`Ord` instances should satisfy the laws of total orderings:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

##### Instances
``` purescript
Ord Boolean
Ord Int
Ord Number
Ord String
Ord Char
Ord Unit
Ord Void
(Ord a) => Ord (Array a)
Ord Ordering
```

#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  sub :: a -> a -> a
```

The `Ring` class is for types that support addition, multiplication,
and subtraction operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Additive inverse: `a - a = (zero - a) + a = zero`

##### Instances
``` purescript
Ring Int
Ring Number
Ring Unit
```

#### `Semigroup`

``` purescript
class Semigroup a where
  append :: a -> a -> a
```

The `Semigroup` type class identifies an associative operation on a type.

Instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

One example of a `Semigroup` is `String`, with `(<>)` defined as string
concatenation.

##### Instances
``` purescript
Semigroup String
Semigroup Unit
Semigroup Void
(Semigroup s') => Semigroup (s -> s')
Semigroup (Array a)
```

#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
```

A `Semigroupoid` is similar to a [`Category`](#category) but does not
require an identity element `id`, just composable morphisms.

`Semigroupoid`s must satisfy the following law:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`,
with `(<<<)` defined as function composition.

##### Instances
``` purescript
Semigroupoid Function
```

#### `Semiring`

``` purescript
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a
```

The `Semiring` class is for types that support an addition and
multiplication operation.

Instances must satisfy the following laws:

- Commutative monoid under addition:
  - Associativity: `(a + b) + c = a + (b + c)`
  - Identity: `zero + a = a + zero = a`
  - Commutative: `a + b = b + a`
- Monoid under multiplication:
  - Associativity: `(a * b) * c = a * (b * c)`
  - Identity: `one * a = a * one = a`
- Multiplication distributes over addition:
  - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
  - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
- Annihiliation: `zero * a = a * zero = zero`

##### Instances
``` purescript
Semiring Int
Semiring Number
Semiring Unit
```

#### `Show`

``` purescript
class Show a where
  show :: a -> String
```

The `Show` type class represents those types which can be converted into
a human-readable `String` representation.

While not required, it is recommended that for any expression `x`, the
string `show x` be executable PureScript code which evaluates to the same
value as the expression `x`.

##### Instances
``` purescript
Show Boolean
Show Int
Show Number
Show Char
Show String
(Show a) => Show (Array a)
```

#### `when`

``` purescript
when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
```

Perform a applicative action when a condition is true.

#### `void`

``` purescript
void :: forall f a. Functor f => f a -> f Unit
```

The `void` function is used to ignore the type wrapped by a
[`Functor`](#functor), replacing it with `Unit` and keeping only the type
information provided by the type constructor itself.

`void` is often useful when using `do` notation to change the return type
of a monadic computation:

```purescript
main = forE 1 10 \n -> void do
  print n
  print (n * n)
```

#### `unless`

``` purescript
unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
```

Perform a applicative action unless a condition is true.

#### `unit`

``` purescript
unit :: Unit
```

`unit` is the sole inhabitant of the `Unit` type.

#### `otherwise`

``` purescript
otherwise :: Boolean
```

An alias for `true`, which can be useful in guard clauses:

```purescript
max x y | x >= y    = x
        | otherwise = y
```

#### `notEq`

``` purescript
notEq :: forall a. Eq a => a -> a -> Boolean
```

`notEq` tests whether one value is _not equal_ to another. Shorthand for
`not (eq x y)`.

#### `negate`

``` purescript
negate :: forall a. Ring a => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `min`

``` purescript
min :: forall a. Ord a => a -> a -> a
```

Take the minimum of two values. If they are considered equal, the first
argument is chosen.

#### `max`

``` purescript
max :: forall a. Ord a => a -> a -> a
```

Take the maximum of two values. If they are considered equal, the first
argument is chosen.

#### `liftM1`

``` purescript
liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
```

`liftM1` provides a default implementation of `(<$>)` for any
[`Monad`](#monad), without using `(<$>)` as provided by the
[`Functor`](#functor)-[`Monad`](#monad) superclass relationship.

`liftM1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftM1
```

#### `liftA1`

``` purescript
liftA1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
```

`liftA1` provides a default implementation of `(<$>)` for any
[`Applicative`](#applicative) functor, without using `(<$>)` as provided
by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
relationship.

`liftA1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftA1
```

#### `join`

``` purescript
join :: forall a m. Bind m => m (m a) -> m a
```

Collapse two applications of a monadic type constructor into one.

#### `ifM`

``` purescript
ifM :: forall a m. Bind m => m Boolean -> m a -> m a -> m a
```

Execute a monadic action if a condition holds.

For example:

```purescript
main = ifM ((< 0.5) <$> random)
         (trace "Heads")
         (trace "Tails")
```

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `const`

``` purescript
const :: forall a b. a -> b -> a
```

Returns its first argument and ignores its second.

```purescript
const 1 "hello" = 1
```

#### `comparing`

``` purescript
comparing :: forall a b. Ord b => (a -> b) -> (a -> a -> Ordering)
```

Compares two values by mapping them to a type with an `Ord` instance.

#### `clamp`

``` purescript
clamp :: forall a. Ord a => a -> a -> a -> a
```

Clamp a value between a minimum and a maximum. For example:

``` purescript
let f = clamp 0 10
f (-5) == 0
f 5    == 5
f 15   == 10
```

#### `between`

``` purescript
between :: forall a. Ord a => a -> a -> a -> Boolean
```

Test whether a value is between a minimum and a maximum (inclusive).
For example:

``` purescript
let f = between 0 10
f 0    == true
f (-5) == false
f 5    == true
f 10   == true
f 15   == false
```

#### `ap`

``` purescript
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
```

`ap` provides a default implementation of `(<*>)` for any
[`Monad`](#monad), without using `(<*>)` as provided by the
[`Apply`](#apply)-[`Monad`](#monad) superclass relationship.

`ap` can therefore be used to write [`Apply`](#apply) instances as
follows:

```purescript
instance applyF :: Apply F where
  apply = ap
```

#### `absurd`

``` purescript
absurd :: forall a. Void -> a
```

#### `(||)`

``` purescript
infixr 2 Data.HeytingAlgebra.disj as ||
```

#### `(>>>)`

``` purescript
infixr 9 Control.Semigroupoid.composeFlipped as >>>
```

#### `(>>=)`

``` purescript
infixl 1 Control.Bind.bind as >>=
```

#### `(>=>)`

``` purescript
infixr 1 Control.Bind.composeKleisli as >=>
```

#### `(>=)`

``` purescript
infixl 4 Data.Ord.greaterThanOrEq as >=
```

#### `(>)`

``` purescript
infixl 4 Data.Ord.greaterThan as >
```

#### `(==)`

``` purescript
infix 4 Data.Eq.eq as ==
```

#### `(=<<)`

``` purescript
infixl 1 Control.Bind.bindFlipped as =<<
```

#### `(<>)`

``` purescript
infixr 5 Data.Semigroup.append as <>
```

#### `(<=<)`

``` purescript
infixr 1 Control.Bind.composeKleisliFlipped as <=<
```

#### `(<=)`

``` purescript
infixl 4 Data.Ord.lessThanOrEq as <=
```

#### `(<<<)`

``` purescript
infixr 9 Control.Semigroupoid.compose as <<<
```

#### `(<*>)`

``` purescript
infixl 4 Control.Apply.apply as <*>
```

#### `(<*)`

``` purescript
infixl 4 Control.Apply.applyFirst as <*
```

#### `(<$>)`

``` purescript
infixl 4 Data.Functor.map as <$>
```

#### `(<$)`

``` purescript
infixl 4 Data.Functor.voidRight as <$
```

#### `(<#>)`

``` purescript
infixl 1 Data.Functor.mapFlipped as <#>
```

#### `(<)`

``` purescript
infixl 4 Data.Ord.lessThan as <
```

#### `(/=)`

``` purescript
infix 4 Data.Eq.notEq as /=
```

#### `(/)`

``` purescript
infixl 7 Data.EuclideanRing.div as /
```

#### `(-)`

``` purescript
infixl 6 Data.Ring.sub as -
```

#### `(+)`

``` purescript
infixl 6 Data.Semiring.add as +
```

#### `(*>)`

``` purescript
infixl 4 Control.Apply.applySecond as *>
```

#### `(*)`

``` purescript
infixl 7 Data.Semiring.mul as *
```

#### `(&&)`

``` purescript
infixr 3 Data.HeytingAlgebra.conj as &&
```

#### `($>)`

``` purescript
infixl 4 Data.Functor.voidLeft as $>
```

#### `($)`

``` purescript
infixr 0 Data.Function.apply as $
```

Applies a function to an argument: the reverse of `(#)`.

```purescript
length $ groupBy productCategory $ filter isInStock $ products
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

Or another alternative equivalent, applying chain of composed functions to
a value:

```purescript
length <<< groupBy productCategory <<< filter isInStock $ products
```

#### `(#)`

``` purescript
infixl 1 Data.Function.applyFlipped as #
```

Applies an argument to a function: the reverse of `($)`.

```purescript
products # filter isInStock # groupBy productCategory # length
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

Or another alternative equivalent, applying a value to a chain of composed
functions:

```purescript
products # filter isInStock >>> groupBy productCategory >>> length
```

#### `type (~>)`

``` purescript
infixr 4 type Data.NaturalTransformation.NaturalTransformation as ype (~>
```

