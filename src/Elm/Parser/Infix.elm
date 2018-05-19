module Elm.Parser.Infix exposing (infixDefinition)

import Combine exposing (Parser, choice, or, string, succeed)
import Combine.Num exposing (int)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (prefixOperatorToken)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))


infixDefinition : Parser State Infix
infixDefinition =
    succeed Infix
        |> Combine.andMap infixDirection
        |> Combine.andMap (Layout.layout |> Combine.continueWith int)
        |> Combine.andMap (Layout.layout |> Combine.continueWith prefixOperatorToken)


infixDirection : Parser State InfixDirection
infixDirection =
    choice
        [ succeed Right |> Combine.ignore (string "infixr")
        , succeed Left |> Combine.ignore (or (string "infixl") (string "infix"))
        ]
