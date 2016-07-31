# purescript-echarts

[![Latest release](http://img.shields.io/bower/v/purescript-echarts.svg)](https://github.com/slamdata/purescript-echarts/releases)
[![Build Status](https://travis-ci.org/slamdata/purescript-echarts.svg?branch=master)](https://travis-ci.org/slamdata/purescript-echarts)
[![Dependency Status](https://www.versioneye.com/user/projects/579df5c7aa78d5003c173910/badge.svg?style=flat)](https://www.versioneye.com/user/projects/579df5c7aa78d5003c173910)

Purescript bindings for Baidu's [Enterprise Charts (Echarts) charting library](https://ecomfe.github.io/echarts/doc/doc-en.html).

## API

The API is a low-level, type-safe, straightforward binding to Echarts:

* All effectful Javascript functions (i.e. virtually *all* functions!) are wrapped in the `Eff` monad for the appropriate effect type (`random`, `dom`, etc.).
* All Javascript parameter or return values which may be nullable are wrapped / unwrapped using `Maybe`.
* Purescript record types are used for Javascript objects.
* Javascript's fake sum types are mapped to Purescript's real sum types, which necessitates encoding / decoding.
* Where Echarts accepts arbitrary JSON, Argonaut JSON data types are used (but records are used to capture as much structure as possible).

## Examples

The `examples` directory contains an `index.html`, which contains a demonstration of each of the 12 different chart types, showing how the Purescript Echarts API may be used.

## Tests

This project has no tests. However, all bound functions are exercised in the examples, to ensure the bindings and necessary wrapping, unwrapping, encoding, and decoding has been performed correctly.
