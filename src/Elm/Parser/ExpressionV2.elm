module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Parser exposing (Parser)
import Pratt


expression : Parser Expression
expression =
    Pratt.expression
        { oneOf = [ Pratt.literal digits ]
        , andThenOneOf = []
        , spaces = Parser.succeed ()
        }
        |> Parser.map node


digits : Parser Expression
digits =
    Parser.number
        { int = Just Expression.IntegerLiteral
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Nothing
        }
