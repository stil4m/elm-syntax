module V7_3_1.Elm.Parser.Infix exposing (infixDefinition)

import Parser as Core
import V7_3_1.Combine as Combine exposing (Parser, choice, string, succeed)
import V7_3_1.Combine.Num exposing (int)
import V7_3_1.Elm.Parser.Layout as Layout
import V7_3_1.Elm.Parser.Node as Node
import V7_3_1.Elm.Parser.State exposing (State)
import V7_3_1.Elm.Parser.Tokens exposing (prefixOperatorToken)
import V7_3_1.Elm.Syntax.Infix exposing (Infix, InfixDirection(..))


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
        |> Combine.andMap (Node.parser V7_3_1.Elm.Parser.Tokens.functionName)


infixDirection : Parser State InfixDirection
infixDirection =
    choice
        [ succeed Right |> Combine.ignore (string "right")
        , succeed Left |> Combine.ignore (string "left")
        , succeed Non |> Combine.ignore (string "non")
        ]
