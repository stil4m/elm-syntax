module Elm.Parser.Infix exposing (infixDefinition)

import Combine exposing ((*>), (<$), (<*>), Parser, choice, or, string, succeed)
import Combine.Num exposing (int)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (prefixOperatorToken)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))


infixDefinition : Parser State Infix
infixDefinition =
    succeed Infix
        <*> infixDirection
        <*> (Layout.layout *> int)
        <*> (Layout.layout *> prefixOperatorToken)


infixDirection : Parser State InfixDirection
infixDirection =
    choice
        [ Right <$ string "infixr"
        , Left <$ or (string "infixl") (string "infix")
        ]
