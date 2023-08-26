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

import Parser.Advanced
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , andThen
        , lazy
        , loop
        , map
        , oneOf
        , succeed
        )



-- PARSER CONFIGURATION


{-| An opaque type holding the parser configuration.
-}
type Config c x e
    = Config
        { oneOf : List (Config c x e -> Parser c x e)
        , andThenOneOf : List (Config c x e -> ( Int, e -> Parser c x e ))
        , spaces : Parser c x ()
        }



-- PRATT PARSER


{-| Just like [`Pratt.expression`](Pratt#expression).
-}
expression :
    { oneOf : List (Config c x e -> Parser c x e)
    , andThenOneOf : List (Config c x e -> ( Int, e -> Parser c x e ))
    , spaces : Parser c x ()
    }
    -> Parser c x e
expression config =
    subExpression 0 <|
        Config
            { oneOf = config.oneOf
            , andThenOneOf = config.andThenOneOf
            , spaces = config.spaces
            }


{-| Just like [`Pratt.subExpression`](Pratt#subExpression).
-}
subExpression : Int -> Config c x e -> Parser c x e
subExpression precedence ((Config conf) as config) =
    succeed identity
        |. conf.spaces
        |= lazy (\_ -> oneOf <| List.map ((|>) config) conf.oneOf)
        |> andThen (\leftExpression -> loop ( config, precedence, leftExpression ) expressionHelp)


{-| This is the core of the Pratt parser algorithm.
It continues parsing the expression as long as a `andThenOneOf` parser with a
precedence above the current one succeeds.

[`loop`](https://package.elm-lang.org/packages/elm/parser/1.1.0/Parser#loop)
is used instead of a recursive parser to get Tail-Call Elimination.

Also note that `oneOf` and `andThenOneOf` parsers may call recursively
`subExpression`.

-}
expressionHelp : ( Config c x e, Int, e ) -> Parser c x (Step ( Config c x e, Int, e ) e)
expressionHelp ( (Config conf) as config, precedence, leftExpression ) =
    succeed identity
        |. conf.spaces
        |= oneOf
            [ map
                (\expr -> Loop ( config, precedence, expr ))
                (operation config precedence leftExpression)
            , succeed (Done leftExpression)
            ]


operation : Config c x e -> Int -> e -> Parser c x e
operation ((Config conf) as config) precedence leftExpression =
    oneOf <|
        List.filterMap
            (\toOperation -> filter (toOperation config) precedence leftExpression)
            conf.andThenOneOf


filter : ( Int, e -> Parser c x e ) -> Int -> e -> Maybe (Parser c x e)
filter ( precedence, parser ) currentPrecedence leftExpression =
    if precedence > currentPrecedence then
        Just (parser leftExpression)

    else
        Nothing



-- ONE_OF HELPERS


{-| Just like [`Pratt.literal`](Pratt#literal).
-}
literal : Parser c x e -> Config c x e -> Parser c x e
literal =
    always


{-| Just like [`Pratt.constant`](Pratt#constant).
-}
constant : Parser c x () -> e -> Config c x e -> Parser c x e
constant constantParser e _ =
    map (always e) constantParser


{-| Just like [`Pratt.prefix`](Pratt#prefix).
-}
prefix : Int -> Parser c x () -> (e -> e) -> Config c x e -> Parser c x e
prefix precedence operator apply config =
    succeed apply
        |. operator
        |= subExpression precedence config



-- AND_THEN_ONE_OF HELPERS


{-| Just like [`Pratt.infixLeft`](Pratt#infixLeft).
-}
infixLeft : Int -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixLeft precedence =
    infixHelp ( precedence, precedence )


{-| Just like [`Pratt.infixRight`](Pratt#infixRight).
-}
infixRight : Int -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixRight precedence =
    -- To get right associativity, we use (precedence - 1) for the
    -- right precedence.
    infixHelp ( precedence, precedence - 1 )


infixHelp : ( Int, Int ) -> Parser c x () -> (e -> e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
infixHelp ( leftPrecedence, rightPrecedence ) operator apply config =
    ( leftPrecedence
    , \left ->
        succeed (apply left)
            |. operator
            |= subExpression rightPrecedence config
    )


{-| Just like [`Pratt.postfix`](Pratt#postfix).
-}
postfix : Int -> Parser c x () -> (e -> e) -> Config c x e -> ( Int, e -> Parser c x e )
postfix precedence operator apply _ =
    ( precedence
    , \left -> map (\_ -> apply left) operator
    )
