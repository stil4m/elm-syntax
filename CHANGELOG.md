# CHANGELOG

## Unreleased

## Version 7.3.0

- Added [`Elm.Processing.parseToFile`] to parse a file without the need to post-process the file, and deprecated [`Elm.Processing.parse`]
- Added [`Elm.Syntax.Range.empty`] as an alias to [`Elm.Syntax.Range.emptyRange`] which is now deprecated
- Added [`Elm.Syntax.Node.empty`]

## Version 7.2.9

* Fix necessary parenthesis missing from some type annotations (i.e. `Typed Int -> String` vs `Typed (Int -> String)`) in Writer module

## Version 7.2.7

* Fix incorrect range for declarations that have documentation
* Fix incorrect parsing of expressions operators inside `Negation` and `RecordAccess` nodes [#147](https://github.com/stil4m/elm-syntax/pull/147)
* Speed up the post-processing step by removing an unnecessary visit of the entire AST
* Speed up the comment attachment in the post-processing step
* Fix typos and formatting of docs

## Version 7.2.6

* Fix incorrect range for function declarations in let expressions
* Elm.Writer no longer adds two dots instead of one for record accessor functions

## Version 7.2.5

Bug fixes:
* Fixed parsing error for `{}` in patterns

## Version 7.2.4

Bug fixes:
* `infixr` and `infixl` can now be used as identifiers
* Pattern matching with `[]` can now have spaces between the brackets
* Closing parenthesis aligned with the start of patterns in a case expression was not allowed
* Missing whitespace between a type parameter in the `=` sign in a type declaration was not allowed
* Elm.Writer was not escaping Char literals

## Version 7.2.3

Bug fixes:
* Improve parsing of variable names which contain unicode, by using [`miniBill/elm-unicode`](https://package.elm-lang.org/packages/miniBill/elm-unicode/latest/) (thanks @miniBill!)

## Version 7.2.2

Bug fixes:
* Fix inability to parse of type aliases and let destructuring when there was no spacing around the `=` sign [#116](https://github.com/stil4m/elm-syntax/pull/116).
* Fix invalid writing of custom types [#30](https://github.com/stil4m/elm-syntax/issues/30) and [#117](https://github.com/stil4m/elm-syntax/issues/117)

## Version 7.2.1

Bug fixes:
* Fix invalid writing of strings containing `"` quotes [#109](https://github.com/stil4m/elm-syntax/issues/109)

Chores:
- Remove elm-community/json-extra dependency

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

[`Elm.Processing.parseToFile`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.3.0/Elm-Processing#parseToFile
[`Elm.Processing.parse`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Processing#parse
[`Elm.Syntax.Range.empty`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#empty
[`Elm.Syntax.Range.emptyRange`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.3.0/Elm-Syntax-Range#emptyRange
[`Elm.Syntax.Node.empty`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Node#empty