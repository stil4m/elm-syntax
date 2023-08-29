module Elm.Parser.Infix exposing (infixDefinition)

import Combine exposing (Parser, oneOf, string, succeed)
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
        |> Combine.keep (Node.parser infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser (Combine.fromCore Core.int))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser <| Combine.parens prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (string "=")
        |> Combine.ignore Layout.layout
        |> Combine.keep (Node.parser Elm.Parser.Tokens.functionName)


infixDirection : Parser State InfixDirection
infixDirection =
    oneOf
        [ succeed Right |> Combine.ignore (string "right")
        , succeed Left |> Combine.ignore (string "left")
        , succeed Non |> Combine.ignore (string "non")
        ]
