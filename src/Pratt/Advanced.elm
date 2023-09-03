module Pratt.Advanced exposing
    ( Config, expression
    , subExpression
    , literal, constant, prefix
    , infixLeft, infixRight, postfix
    )

{-| `Pratt.Advanced` provides the same API as [`Pratt`](Pratt),
but for [`Parser.Advanced`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser-Advanced). This allows to have custom `context` and `problem` types to improve error messages.


# Expression parser

@docs Config, expression


# Configuration helpers

@docs subExpression


## **oneOf** helpers

@docs literal, constant, prefix


## **andThenOneOf** helpers

@docs infixLeft, infixRight, postfix

-}

import Combine exposing (Parser, Step(..), map, oneOf, succeed)



-- PARSER CONFIGURATION


{-| An opaque type holding the parser configuration.
-}
type Config s e
    = Config
        { oneOf : List (Config s e -> Parser s e)
        , andThenOneOf : List (Config s e -> ( Int, e -> Parser s e ))
        , spaces : Parser s ()
        }



-- PRATT PARSER


{-| Just like [`Pratt.expression`](Pratt#expression).
-}
expression :
    { oneOf : List (Config s e -> Parser s e)
    , andThenOneOf : List (Config s e -> ( Int, e -> Parser s e ))
    , spaces : Parser s ()
    }
    -> Parser s e
expression config =
    subExpression 0 <|
        Config
            { oneOf = config.oneOf
            , andThenOneOf = config.andThenOneOf
            , spaces = config.spaces
            }


{-| Just like [`Pratt.subExpression`](Pratt#subExpression).
-}
subExpression : Int -> Config s e -> Parser s e
subExpression precedence ((Config conf) as config) =
    conf.spaces
        |> Combine.continueWith
            (Combine.lazy
                (\_ ->
                    oneOf <| List.map (\e -> e config) conf.oneOf
                )
            )
        |> Combine.andThen (\leftExpression -> Combine.loop ( config, precedence, leftExpression ) expressionHelp)


{-| This is the core of the Pratt parser algorithm.
It continues parsing the expression as long as a `andThenOneOf` parser with a
precedence above the current one succeeds.

[`loop`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#loop)
is used instead of a recursive parser to get Tail-Call Elimination.

Also note that `oneOf` and `andThenOneOf` parsers may call recursively
`subExpression`.

-}
expressionHelp : ( Config s e, Int, e ) -> Parser s (Step ( Config s e, Int, e ) e)
expressionHelp ( (Config conf) as config, precedence, leftExpression ) =
    succeed identity
        |> Combine.ignore conf.spaces
        |> Combine.keep
            (oneOf
                [ map
                    (\expr -> Loop ( config, precedence, expr ))
                    (operation config precedence leftExpression)
                , succeed (Done leftExpression)
                ]
            )


operation : Config s e -> Int -> e -> Parser s e
operation ((Config conf) as config) precedence leftExpression =
    oneOf <|
        List.filterMap
            (\toOperation -> filter (toOperation config) precedence leftExpression)
            conf.andThenOneOf


filter : ( Int, e -> Parser s e ) -> Int -> e -> Maybe (Parser s e)
filter ( precedence, parser ) currentPrecedence leftExpression =
    if precedence > currentPrecedence then
        Just (parser leftExpression)

    else
        Nothing



-- ONE_OF HELPERS


{-| Just like [`Pratt.literal`](Pratt#literal).
-}
literal : Parser s e -> Config s e -> Parser s e
literal =
    always


{-| Just like [`Pratt.constant`](Pratt#constant).
-}
constant : Parser s () -> e -> Config s e -> Parser s e
constant constantParser e _ =
    map (always e) constantParser


{-| Just like [`Pratt.prefix`](Pratt#prefix).
-}
prefix : Int -> Parser s () -> (e -> e) -> Config s e -> Parser s e
prefix precedence operator apply config =
    succeed apply
        |> Combine.ignore operator
        |> Combine.keep (subExpression precedence config)



-- AND_THEN_ONE_OF HELPERS


{-| Just like [`Pratt.infixLeft`](Pratt#infixLeft).
-}
infixLeft : Int -> Parser s () -> (e -> e -> e) -> Config s e -> ( Int, e -> Parser s e )
infixLeft precedence p apply config =
    infixHelp precedence precedence p apply config


{-| Just like [`Pratt.infixRight`](Pratt#infixRight).
-}
infixRight : Int -> Parser s () -> (e -> e -> e) -> Config s e -> ( Int, e -> Parser s e )
infixRight precedence p apply config =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp precedence (precedence - 1) p apply config


infixHelp : Int -> Int -> Parser s () -> (e -> e -> e) -> Config s e -> ( Int, e -> Parser s e )
infixHelp leftPrecedence rightPrecedence operator apply config =
    ( leftPrecedence
    , \left ->
        Combine.succeed (\e -> apply left e)
            |> Combine.ignore operator
            |> Combine.keep (subExpression rightPrecedence config)
    )


{-| Just like [`Pratt.postfix`](Pratt#postfix).
-}
postfix : Int -> Parser s () -> (e -> e) -> Config s e -> ( Int, e -> Parser s e )
postfix precedence operator apply _ =
    ( precedence
    , \left -> map (\_ -> apply left) operator
    )
