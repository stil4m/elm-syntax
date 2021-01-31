# CHANGELOG

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
