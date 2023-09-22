module Pratt exposing
    ( Config, expression
    , subExpression
    , literal, constant, prefix
    , infixLeft, infixRight, postfix, recordAccessPostfix
    )

{-|

  - **Expression parser**: [`expression`](#expression)
  - **Configuration helpers**: [`subExpression`](#subExpression)
      - **oneOf** helpers: [`literal`](#literal) [`constant`](#constant)
        [`prefix`](#prefix)
      - **andThenOneOf** helpers: [`infixLeft`](#infixLeft)
        [`infixRight`](#infixRight) [`postfix`](#postfix)


# Expression parser

@docs Config, expression


# Configuration helpers

@docs subExpression


## **oneOf** helpers

@docs literal, constant, prefix


## **andThenOneOf** helpers

@docs infixLeft, infixRight, postfix, recordAccessPostfix

-}

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node)
import Pratt.Advanced as Advanced



-- EXPRESSION PARSER CONFIGURATION


{-| An opaque type based on
[`Pratt.Advanced.Config`](Pratt.Advanced#Config) holding the parser
configuration.
-}
type alias Config state expr =
    Advanced.Config state expr



-- EXPRESSION PARSER


{-| Build a parser based on the given configuration that can
be combined with other
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser)
parsers.

The top level expression is parsed with a _precedence_ of `0`,
see [`subExpression`](#subExpression).

**`oneOf`:**

> A list of parsers used at the start of an expression (or sub-expression),
> or after an operator.
> They will be tried successively by the parser using
> [`Parser.oneOf`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#oneOf)
> with the current parser `Config` passed as their argument.

> Examples: parsers for _literals_, _constants_, _prefix_ expressions or a
> sub-expression between parentheses.

> At least one of them is required and one must succeed, else the whole
> expression parser would not be able to parse an `expr`.

**`andThenOneOf`:**

> A list of parsers using the result of a previous parser.
> They need to provide their `Int` precedence as only the ones that have a
> _precedence_ above the current one (`0` by default) will be tried by the
> parser using `Parser.oneOf`, with the parser `Config` and the current _left_
> expression (the expression returned by the previous parser) arguments.

> Examples: parsers for _prefix_ and _postfix_ expressions.

**`spaces`:**

> A parser called before and after each previously configured parser, typically
> used to consume whitespaces.

> If a more specific behavior is needed, this parser can be ignored by using
> `succeed ()` and a custom behavior added inside `oneOf` and `andThenOneOf` parsers.

**Notes:**

  - All configured parsers except the `spaces` one are parameterized by a
    [`Config`](#Config) to be able to call [`subExpression`](#subExpression),
    which is the main building block for configuration helpers. This `Config`
    will be automatically passed by the parser.
  - The parser will not use
    [`Parser.backtrackable`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#backtrackable),
    so it is up to you to correctly setup your parsers to avoid having failed
    parsers consuming characters.

For example, a basic calculator could be configured like this:

    import Parser exposing ((|.), (|=), Parser, end, float, keyword, run, succeed, symbol)
    import Pratt exposing (infixLeft, infixRight, literal, prefix)

    mathExpression : Parser Float
    mathExpression =
        Pratt.expression
            { oneOf =
                [ literal float
                , prefix 3 (symbol "-") negate
                , parenthesizedExpression
                ]
            , andThenOneOf =
                [ infixLeft 1 (symbol "+") (+)
                , infixLeft 1 (symbol "-") (-)
                , infixLeft 2 (symbol "*") (*)
                , infixLeft 2 (symbol "/") (/)
                , infixRight 4 (symbol "^") (^)
                ]
            , spaces = Parser.spaces
            }

    parenthesizedExpression : Config Float -> Parser Float
    parenthesizedExpression config =
        succeed identity
            |. symbol "("
            |= subExpression 0 config
            |. symbol ")"

    parser : Parser Float
    parser =
        succeed identity
            |= mathExpression
            |. end


    run parser "-1*3--5+4/2^2" --> Ok ((-1*3)-(-5)+(4/(2^2)))
    run parser "-1*3--5+4/2^2" --> Ok 3
    run parser "((-1*3) - (-5) + (4 / (2^2)))" --> Ok 3

-}
expression :
    { oneOf : List (Config state expr -> Parser state expr)
    , andThenOneOf : List (Config state expr -> ( Int, expr -> Parser state expr ))
    , spaces : Parser state ()
    }
    -> Parser state expr
expression =
    Advanced.expression


{-| Build an expression parser based on the _precedence_ and
configuration arguments.

This is the core function of the parser.
The [`expression`](#expression) function is actually implemented using
`subExpression` with a _precedence_ of `0`.

`subExpression` is used to make configuration helpers.

For example [`prefix`](#prefix) can be implemented like this:

    prefix : Int -> Parser state () -> (expr -> expr) -> Config state expr -> Parser state expr
    prefix precedence operator apply config =
        succeed apply
            |. operator
            |= subExpression precedence config

A parser for sub-expressions between parentheses like this:

    parenthesizedExpression : Config Expr -> Parser Expr
    parenthesizedExpression config =
        succeed identity
            |. symbol "("
            |= subExpression 0 config
            |. symbol ")"

**Algorithm**:

`subExpression` uses the following algorithm:

1.  Use the `Int` _precedence_ argument as the current precedence.
2.  Run the `spaces` parser.
3.  Try configured `oneOf` parsers successively in order using
    [`Parser.oneOf`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#oneOf)
    until one is chosen (i.e. until one of them starts chomping characters).
    This parser may call `subExpression` recursively with different
    _precedence_ values.
    If no parser succeeds, the whole parser fails, else the expression parsed is
    used as the _left_ expression in the next steps.
4.  Loop
      - Run the configured `spaces` parser.
      - Filter `andThenOneOf` parsers, keeping only those that have a
        _precedence_ above the current one, then try them successively using
        `Parser.oneOf` with the _left_ expression argument until one is chosen.
        This parser may also call
        `subExpression` recursively with different _precedence_ values.
      - If no parser succeeds, return the _left_ expression.
        Else, loop from 4. using the expression just parsed as the new _left_
        expression.

-}
subExpression : Int -> Config state expr -> Parser state expr
subExpression =
    Advanced.subExpression



-- ONE_OF HELPERS


{-| Build a parser for a _literal_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, number, run)
    import Pratt exposing (literal)

    type Expr
        = Int Int
        | Float Float

    digits : Parser Expr
    digits =
        number
            { int = Just Int
            , hex = Just Int
            , octal = Nothing
            , binary = Nothing
            , float = Just Float
            }

    expression : Parser Expr
    expression =
        Pratt.expression
            { oneOf = [ literal digits ]
            , andThenOneOf = []
            , spaces = Parser.spaces
            }


    run expression  "1234" --> Ok (Int 1234)
    run expression  "0x1b" --> Ok (Int 27)
    run expression  "3.14159" --> Ok (Float 3.14159)

**Note:** if you want to be able to handle expressions like `3---4 == 3-(-(-4))`,
you could have a negation _prefix_ parser like `prefix 3 (-) Neg` declared
before the `literal digits` and let `digits` only handle positive numbers.

-}
literal : Parser state expr -> Config state expr -> Parser state expr
literal =
    Advanced.literal


{-| Build a parser for a _constant_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, keyword, run)
    import Pratt exposing (constant)

    expression : Parser Float
    expression =
        Pratt.expression
            { oneOf = [ constant (keyword "pi") pi ]
            , andThenOneOf = []
            , spaces = Parser.spaces
            }


    run expression "pi" --> Ok pi

-}
constant : Parser state () -> expr -> Config state expr -> Parser state expr
constant =
    Advanced.constant


{-| Build a parser for a _prefix_ expression with a given _precedence_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, int, run, symbol)
    import Pratt exposing (literal, prefix)

    expression : Parser Int
    expression =
        Pratt.expression
            { oneOf =
                [ literal int
                , prefix 3 (symbol "-") negate
                , prefix 3 (symbol "+") identity
                ]
            , andThenOneOf = []
            , spaces = Parser.spaces
            }


    run expression "-1" --> Ok -1
    run expression "--1" --> Ok 1
    run expression "+1" --> Ok 1
    run expression "+-+-1" --> Ok 1

`prefix` can also be used to build more complex helpers, for example:

    type Expr
        = IfThenElse Expr Expr Expr

    ifThenElse : Config Expr -> Parser Expr
    ifThenElse config =
        succeed IfThenElse
            |= prefix 0 (keyword "if") identity config
            |= prefix 0 (keyword "then") identity config
            |= prefix 0 (keyword "else") identity config

-}
prefix : Int -> Parser state () -> (expr -> expr) -> Config state expr -> Parser state expr
prefix =
    Advanced.prefix



-- AND_THEN_ONE_OF HELPERS


{-| Build a parser for an _infix_ expression with a left-associative operator
and a given _precedence_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, float, run, symbol)
    import Pratt exposing (literal, prefix)

    expression : Parser Float
    expression =
        Pratt.expression
            { oneOf = [ literal float ]
            , andThenOneOf =
                [ infixLeft 1 (symbol "+") (+)
                , infixLeft 1 (symbol "-") (-)
                , infixLeft 2 (symbol "*") (*)
                , infixLeft 2 (symbol "/") (/)
                ]
            , spaces = Parser.spaces
            }


    run expression "5+4-3*2/1" --> Ok (5+4-(3*2/1))
    run expression "5+4-3*2/1" --> Ok 3

-}
infixLeft : Int -> Parser state () -> (expr -> expr -> expr) -> Config state expr -> ( Int, expr -> Parser state expr )
infixLeft =
    Advanced.infixLeft


{-| Build a parser for an _infix_ expression with a right-associative operator
and a given _precedence_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, float, run, symbol)
    import Pratt exposing (literal, infixRight)

    expression : Parser Float
    expression =
        Pratt.expression
            { oneOf = [ literal float ]
            , andThenOneOf = [ infixRight 4 (symbol "^") (^) ]
            , spaces = Parser.spaces
            }


    run expression "2^2^3" --> Ok (2^(2^3))
    run expression "2^2^3" --> Ok 256

**Note:** As usual in Pratt parsers, right-associativity is achieved by parsing the right
expression with the _precedence_ of the infix operator minus 1.

-}
infixRight : Int -> Parser state () -> (expr -> expr -> expr) -> Config state expr -> ( Int, expr -> Parser state expr )
infixRight =
    Advanced.infixRight


{-| Build a parser for a _postfix_ expression with a given _precedence_.

The `Config` argument is passed automatically by the parser.

    import Parser exposing (Parser, float, run, symbol)
    import Pratt exposing (literal, postfix)

    expression : Parser Float
    expression =
        Pratt.expression
            { oneOf = [ literal float ]
            , andThenOneOf = [ postfix 6 (symbol "°") degrees ]
            , spaces = Parser.spaces
            }


    run expression "180°" --> Ok pi
    run expression "360°" --> Ok (2*pi)

-}
postfix : Int -> Parser state () -> (expr -> expr) -> Config state expr -> ( Int, expr -> Parser state expr )
postfix =
    Advanced.postfix


recordAccessPostfix : Int -> Parser s (Node String) -> (e -> Node String -> e) -> Advanced.Config s e -> ( Int, e -> Parser s e )
recordAccessPostfix =
    Advanced.recordAccessPostfix
