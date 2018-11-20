module Elm.Parser.Infix exposing (infixDefinition)

import Combine exposing (Parser, choice, string, succeed)
import Combine.Num exposing (int)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (prefixOperatorToken)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Parser as Core


infixDefinition : Parser State Infix
infixDefinition =
    succeed Infix
        |> Combine.ignore (Combine.fromCore (Core.keyword "infix"))
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser int)
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser <| Combine.parens prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.andMap (Node.parser Elm.Parser.Tokens.functionName)


infixDirection : Parser State InfixDirection
infixDirection =
    choice
        [ succeed Right |> Combine.ignore (string "right")
        , succeed Left |> Combine.ignore (string "left")
        , succeed Non |> Combine.ignore (string "non")
        ]
