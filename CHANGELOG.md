# CHANGELOG

## [Unreleased]

- Removed the deprecated `Elm.Parser.parse` function, and renamed `Elm.Parser.parseToFile` to `Elm.Parser.parse` (in other words, `Elm.Parser.parse`'s type changed to be the same as `parseToFile`)

- Removed APIs:
  - `Elm.Writer` module
  - `Elm.Interface` module
  - `Elm.Dependency` module
  - `Elm.Processing` and `Elm.RawFile` modules
  - All `encode` and `decode` functions

- Changed `Elm.Syntax.Declaration.Declaration`:
  - Removed `Destructuring` variant (it was not possible to get)
  - `PortDeclaration` documentation's is now attached to the declaration
    - `PortDeclaration Signature` -> `PortDeclaration Port` where `type alias Port = { documentation : Maybe (Node Documentation), signature : Signature }`
    - The comment for the port declaration is now removed from the `Elm.Syntax.File.File.comments` field.

- Add a new module `Elm.Syntax.DeconstructPattern` which mimics `Elm.Syntax.Pattern` but for patterns in functions and let declarations.
  - This removes the need to handle impossible patterns. For instance, you can't find a string pattern in `someFn (X "impossible") = x` in a function declaration.

- Changed `Elm.Syntax.Expression.Expression`:
  - **WARNING (will not result in compilation error)** Stripped the `.` in the `String` stored in `RecordAccessFunction` (for code like `.field`)
    - Example: `RecordAccessFunction ".field"` -> `RecordAccessFunction "field"`
  - Added information to `String` literal as to whether `"""` or `"` was used:
    - `Literal String` -> `Literal StringLiteralType String`
    - Added `type StringLiteralType = TripleQuote | SingleQuote`
  - `Application (List (Node Expression))` -> `Application (Node Expression) (List (Node Expression))` (takes a non-empty list of arguments)
  - `RecordUpdateExpression (Node String) (List (Node RecordSetter))` -> `RecordUpdateExpression (Node String) (Node RecordSetter) (List (Node RecordSetter))` (takes a non-empty list of fields)
  - Renamed `TupledExpression` to `TupleExpression`
  - Renamed `IfBlock` to `If`
  - Renamed `Floatable` to `FloatLiteral`
  - `UnitExpr` -> `TupleExpression []`
  - `ParenthesizedExpression x` -> `TupleExpression [ x ]`
  - Removed `Elm.Syntax.Expression.Cases` type alias (it was `type alias Cases = List Case`)
  - `Elm.Syntax.Expression.CaseBlock`'s `cases : List Case` field is split into `firstCase : Case` and `restOfCases : List Case` (takes a non-empty list of cases)

- Changed `Elm.Syntax.Pattern.Pattern`:
  - Removed `FloatPattern` (it was not possible to get)

- Changed `Elm.Syntax.TypeAnnotation.TypeAnnotation`:
  - `Tupled` -> `Tuple`
  - `Unit` -> `Tuple []`
  - Renamed `Typed` to `Type`
  - `Elm.Syntax.TypeAnnotation.Type`'s `constructors : List (Node ValueConstructor)` field is split into `firstConstructor : Node ValueConstructor` and `restOfConstructors : List (Node ValueConstructor)` (takes a non-empty list of constructors)
  - Renamed `GenericType` to `Var`

- Changed `Elm.Syntax.Exposing.Exposing`:
  - `Explicit (List (Node TopLevelExpose))` -> `Explicit (Node TopLevelExpose) (List (Node TopLevelExpose))` (takes a non-empty list of elements)

- Added module `Elm.Syntax.StringLiteralType` containing `type StringLiteralType = TripleQuote | SingleQuote`

## [7.3.4] - 2024-07-26

Parsing is faster by ~90%. A big thank you to [@lue-bird](https://github.com/lue-bird) for finding and introducing a huge amount of performance improvements.

## [7.3.3] - 2024-07-18

### Parser rewrite using a Pratt parser

This patch is a major rewrite of the parser, which now uses a Pratt parser under the hood (see https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html for an explanation).

Prior to this version, the source code was parsed and then post-processed - to rebalance the tree around operator precedence and to attach documentation comments.
This is now all done in a single pass, which improves performance by about 15%.

This rewrite includes a few additional changes:
- a few bug fixes where code that was incorrect Elm code (according to the Elm compiler) was successfully parsed
- better performance, parsing should now be faster
- support for multiline strings in patterns

A big thank you to [@jiegillet](https://github.com/jiegillet) and [@janiczek](https://github.com/janiczek) for their help.

### Deprecation of Elm.Writer

The `Elm.Writer` module has never been very good and tended to, and we do not desire maintaining it, and it will therefore likely be removed in a future major version.
We highly recommend using [the-sett/elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/) instead, which is much better anyways.

## [7.3.2] - 2022-09-03

- Stop parsing `FloatPattern` (this is an invalid pattern in Elm 0.19)
- Stop parsing `Destructuring` declaration (this is an invalid declaration in Elm 0.19)
- Fail to parse when doing exposing clause is empty (`exposing ()`)
- Fail to parse when encountering an annotation for the wrong declaration (`a : T` followed by `b = 1`)
- Various performance improvements that make the parser 6% to 9% faster

## [7.3.1] - 2022-08-16

- Fixed an infinite recursion during parsing of a file.

## [7.3.0] - 2023-08-16

- Added [`Elm.Parser.parseToFile`] to parse a file without the need to post-process the file, and deprecated [`Elm.Parser.parse`]
- Added [`Elm.Syntax.Range.empty`] as an alias to [`Elm.Syntax.Range.emptyRange`] which is now deprecated
- Added [`Elm.Syntax.Node.empty`]

## [7.2.9] - 2022-02-11

- Fix necessary parenthesis missing from some type annotations (i.e. `Typed Int -> String` vs `Typed (Int -> String)`) in Writer module

## [7.2.8] - 2021-10-14

- Fixed an issue with parsing lambda expressions

## [7.2.7] - 2021-08-14

- Fix incorrect range for declarations that have documentation
- Fix incorrect parsing of expressions operators inside `Negation` and `RecordAccess` nodes [#147](https://github.com/stil4m/elm-syntax/pull/147)
- Speed up the post-processing step by removing an unnecessary visit of the entire AST
- Speed up the comment attachment in the post-processing step
- Fix typos and formatting of docs

## [7.2.6] - 2021-07-17

- Fix incorrect range for function declarations in let expressions
- Elm.Writer no longer adds two dots instead of one for record accessor functions

## [7.2.5] - 2021-05-20

Bug fixes:
- Fixed parsing error for `{}` in patterns

## [7.2.4] - 2021-05-10

Bug fixes:
- `infixr` and `infixl` can now be used as identifiers
- Pattern matching with `[]` can now have spaces between the brackets
- Closing parenthesis aligned with the start of patterns in a case expression was not allowed
- Missing whitespace between a type parameter in the `=` sign in a type declaration was not allowed
- Elm.Writer was not escaping Char literals

## [7.2.3] - 2021-03-30

Bug fixes:
- Improve parsing of variable names which contain unicode, by using [`miniBill/elm-unicode`](https://package.elm-lang.org/packages/miniBill/elm-unicode/latest/) (thanks @miniBill!)

## [7.2.2] - 2021-03-25

Bug fixes:
- Fix inability to parse of type aliases and let destructuring when there was no spacing around the `=` sign [#116](https://github.com/stil4m/elm-syntax/pull/116).
- Fix invalid writing of custom types [#30](https://github.com/stil4m/elm-syntax/issues/30) and [#117](https://github.com/stil4m/elm-syntax/issues/117)

## [7.2.1] - 2021-02-07

Bug fixes:
- Fix invalid writing of strings containing `"` quotes [#109](https://github.com/stil4m/elm-syntax/issues/109)

Chores:
- Remove elm-community/json-extra dependency

## [7.2.0] - 2021-01-31

API changes:
- Added [`Elm.Syntax.Range.compare`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.0/Elm-Syntax-Range#compare).
- Added [`Elm.Syntax.Range.compareLocations`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.0/Elm-Syntax-Range#compareLocations).

Bug fixes:
- Fix wrong parsing of multiline strings [#89](https://github.com/stil4m/elm-syntax/issues/89).
- Fix `Elm.Writer` writing invalid case expressions [#98](https://github.com/stil4m/elm-syntax/issues/98).
- Fix operator associativity when creating the AST [#87](https://github.com/stil4m/elm-syntax/issues/87).
- Fix parsing of non-unicode characters [#47](https://github.com/stil4m/elm-syntax/issues/47).
- Fix Elm code using invalid unary operators from being parsed [#62](https://github.com/stil4m/elm-syntax/issues/62).
- Fix the range for let expressions ([#63](https://github.com/stil4m/elm-syntax/issues/63)) and if expressions ([#94](https://github.com/stil4m/elm-syntax/issues/94).
- Fix the range for `TypeOrAliasExposing` [#46](https://github.com/stil4m/elm-syntax/issues/46).
- Ordered the list of a file's comments in the same order that they appear in [#89](https://github.com/stil4m/elm-syntax/issues/89).

## [7.1.3] - 2020-06-17

- Fix the range for `RecordAccess` expressions [#36](https://github.com/stil4m/elm-syntax/pull/36).

## [7.1.2] - 2020-06-15

- Take operator precedence into account when creating the AST [#41](https://github.com/stil4m/elm-syntax/issues/41).

## [7.1.1] - 2019-10-25

- Improve stability for very large case statements.

## 7.x.x

This version implements compatibility to parse source files for Elm 0.19.
Elm 0.18 is not supported from this point on.

With this change some additional big changes were made to the projects and the design.

- The `elm-community/parser-combinators` library was exchanged for `elm/parser`. This should give a significant performance improvement.
- Parse errors produced by `Elm.Parser` will fallback on the errors that `elm/parser` provides.
- The type `Ranged` is eliminated and replaced with `Node`. A node is just a wrapper for an AST element with a specific range (the area in the input that the AST node covers).
- Range information is added to a significant bigger set of AST elements.
- The decoders and encoders for the specific AST elements are moved to their modules (`Elm.Syntax.*`).

[Unreleased]: https://github.com/stil4m/elm-syntax/compare/v7.3.4...HEAD
[7.3.4]: https://github.com/stil4m/elm-syntax/releases/tag/7.3.4
[7.3.3]: https://github.com/stil4m/elm-syntax/releases/tag/7.3.3
[7.3.2]: https://github.com/stil4m/elm-syntax/releases/tag/7.3.2
[7.3.1]: https://github.com/stil4m/elm-syntax/releases/tag/7.3.1
[7.3.0]: https://github.com/stil4m/elm-syntax/releases/tag/7.3.0
[7.2.9]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.9
[7.2.8]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.8
[7.2.7]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.7
[7.2.6]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.6
[7.2.5]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.5
[7.2.4]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.4
[7.2.3]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.3
[7.2.2]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.2
[7.2.1]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.1
[7.2.0]: https://github.com/stil4m/elm-syntax/releases/tag/7.2.0
[7.1.3]: https://github.com/stil4m/elm-syntax/releases/tag/7.1.3
[7.1.2]: https://github.com/stil4m/elm-syntax/releases/tag/7.1.2
[7.1.1]: https://github.com/stil4m/elm-syntax/releases/tag/7.1.1

[`Elm.Processing.parseToFile`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.3.0/Elm-Processing#parseToFile
[`Elm.Processing.parse`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Processing#parse
[`Elm.Syntax.Range.empty`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#empty
[`Elm.Syntax.Range.emptyRange`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.3.0/Elm-Syntax-Range#emptyRange
[`Elm.Syntax.Node.empty`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Node#empty