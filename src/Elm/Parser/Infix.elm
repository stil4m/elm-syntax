module Elm.Parser.Infix exposing (infixDefinition)

import Combine exposing (Parser, choice, or, string, succeed)
import Combine.Num exposing (int)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (prefixOperatorToken)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))


infixDefinition : Parser State Infix
infixDefinition =
    succeed Infix
        |> Combine.ignore (string "infix")
        |> Combine.ignore Layout.layout
        |> Combine.andMap (ranged infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.andMap (ranged int)
        |> Combine.ignore Layout.layout
        |> Combine.andMap (ranged <| Combine.parens prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.andMap (ranged Elm.Parser.Tokens.functionName)


infixDirection : Parser State InfixDirection
infixDirection =
    choice
        [ succeed Right |> Combine.ignore (string "right")
        , succeed Left |> Combine.ignore (string "left")
        ]
