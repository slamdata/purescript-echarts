## Module ECharts.Monad

#### `DSLMonad`

``` purescript
newtype DSLMonad (i :: # !) a
  = DSL (Writer (Array (Tuple String Foreign)) a)
```

##### Instances
``` purescript
Functor (DSLMonad i)
Apply (DSLMonad i)
Applicative (DSLMonad i)
Bind (DSLMonad i)
Monad (DSLMonad i)
```

#### `DSL`

``` purescript
type DSL i = DSLMonad i Unit
```

#### `set`

``` purescript
set :: forall i. String -> Foreign -> DSL i
```

#### `buildObj`

``` purescript
buildObj :: forall i. DSL i -> Foreign
```

#### `buildSeries`

``` purescript
buildSeries :: forall i. DSL i -> Foreign
```

#### `buildArr`

``` purescript
buildArr :: forall i. DSL i -> Foreign
```


