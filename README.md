![travis build status](https://travis-ci.org/cryogenian/purescript-echarts.svg?branch=master)
# purescript-echarts

Purescript bindings for Baidu's [Enterprise Charts (Echarts) charting library](https://github.com/ecomfe/echarts).

## API

This approach is drastically differs with __v2.0.0__.

All basic echarts function are implemented as effectful functions. They are wrapped in `MonadEff` typeclass and could be used not only in `Eff` monad but in `Aff` and effectful transformers.

`Option` object is constructed using `Writer` monad augmented with phantom rows. I.e. there is no any
special type for `Option`, `Legend` or `Tooltip`, but they are `DSL PhantomRowsI` where `PhantomRowsI` is phantom row type.

To write a field to objects one should use function from `ECharts.Commands` module. Row label of this function indicates if this function could be used for building particular objects. For example, if you build something like `Pie` series object you can use function `name` because it has type `forall i. String -> DSL (name :: I|i)` and `PieI` has this `name` label in its phantom part, but you can't use `symbol` because its type is `forall i. Symbol -> DSL (symbol :: I|i)` and `PieI` hasn't `symbol` label.

## Examples

To run examples
```bash
npm run build
npm run serve
```
Then go to `localhost:5050`

## Notes

* To use this lib in your project you need to add `echarts` npm dependency to your project.
* This branch isn't fully implemented yet, although it has all features that we currently use in slamdata.
* `Writer`'s log is `Array (Tuple String Foreign)`. There are three different functions that builds sub-dsls: `buildObj` takes pairs from log and write `Foreign`s as values to empty object; `buildArr` ignores first part of tuple and just write `Foreign`s to empty array, `buildSeries` takes first part of tuple and write it as `"type"` field to the second part (`Tuple "foo" $ toForeign {a: 12} -> toForeign {"type": "foo", a: 12}`) and then put it to empty array.
* There are row names that differs with field names from __echarts__ docs. For example, __echarts__ uses `data` field and `purescript-echarts` uses `items`. Other thing is that some field names collide and to fix it there are new row names: `normalItemStyle`, `normalLabel`.
* In this approach one could add new type for some field value (like `PercentOrPixel`) _OR_ add command (like `leftCenter`) _OR_ add new `DSL` for building foreign data (like `addItem`).
