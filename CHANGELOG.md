# CHANGELOG

## Version 7.2.0

API changes:
* Added [`Elm.Syntax.Range.compare`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.0/Elm-Syntax-Range#compare).
* Added [`Elm.Syntax.Range.compareLocations`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.0/Elm-Syntax-Range#compareLocations).

Bug fixes:
* Fix wrong parsing of multiline strings [#89](https://github.com/stil4m/elm-syntax/issues/89).
* Fix `Elm.Writer` writing invalid case expressions [#98](https://github.com/stil4m/elm-syntax/issues/98).
* Fix operator associativity when creating the AST [#87](https://github.com/stil4m/elm-syntax/issues/87).
* Fix parsing of non-unicode characters [#47](https://github.com/stil4m/elm-syntax/issues/47).
* Fix Elm code using invalid unary operators from being parsed [#62](https://github.com/stil4m/elm-syntax/issues/62).
* Fix the range for let expressions ([#63](https://github.com/stil4m/elm-syntax/issues/63)) and if expressions ([#94](https://github.com/stil4m/elm-syntax/issues/94).
* Fix the range for `TypeOrAliasExposing` [#46](https://github.com/stil4m/elm-syntax/issues/46).
* Ordered the list of a file's comments in the same order that they appear in [#89](https://github.com/stil4m/elm-syntax/issues/89).

## Version 7.1.3

* Fix the range for `RecordAccess` expressions [#36](https://github.com/stil4m/elm-syntax/pull/36).

## Version 7.1.2

* Take operator precedence into account when creating the AST [#41](https://github.com/stil4m/elm-syntax/issues/41).

## Version 7.1.1

* Improve stability for very large case statements.

## Version 7.x.x

This version implements compatibility to parse source files for Elm 0.19.
Elm 0.18 is not supported from this point on.

With this change some additional big changes were made to the projects and the design.

* The `elm-community/parser-combinators` library was exchanged for `elm/parser`. This should give a significant performance improvement.
* Parse errors produced by `Elm.Parser` will fallback on the errors that `elm/parser` provides.
* The type `Ranged` is eliminated and replaced with `Node`. A node is just a wrapper for an AST element with a specific range (the area in the input that the AST node covers).
* Range information is added to a significant bigger set of AST elements.
* The decoders and encoders for the specific AST elements are moved to their modules (`Elm.Syntax.*`).
