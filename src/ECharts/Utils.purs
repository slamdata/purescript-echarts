module ECharts.Utils where

import Data.Argonaut.Core (Json)

{-
This func is used to construct copy of object, without null and undefined fields.
i.e
{foo: 1, bar: 12, baz: null, quux: undefined} ->
{foo: 1, bar: 12}
-}
foreign import unnull ∷ Json → Json
