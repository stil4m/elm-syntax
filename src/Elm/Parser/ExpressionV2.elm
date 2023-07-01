module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (Parser)
import Pratt


expression : Parser (Node Expression)
expression =
    Pratt.expression
        { oneOf = []
        , andThenOneOf = []
        , spaces = Parser.succeed ()
        }
