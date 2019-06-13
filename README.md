# bitstring [![Build Status](https://travis-ci.com/zimmydev/bitstring.svg?branch=master)](https://travis-ci.com/zimmydev/bitstring)

A library for working with bits and bitstrings in elm.

# Documentation

* [`Bitstring`](https://package.elm-lang.org/packages/zimmydev/bitstring/latest/Bitstring)
* [`Bitstring.Parser`](https://package.elm-lang.org/packages/zimmydev/bitstring/latest/Bitstring-Parser)

# Installation

From your top-level project directory (the one with `elm.json`), run:

```
elm install zimmydev/bitstring
```

# Verifying examples

Note: This requires installing the
[`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples) package
from npm.

Travis-CI doesn't include support for `elm-verify-examples`, so you'll need to
run it manually. From your top-level project directory, run:

```
elm-verify-examples && elm-test
```
