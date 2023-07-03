module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Parser exposing ((|=), Parser)
import Pratt


expression : Parser (Node Expression)
expression =
    Pratt.expression
        { oneOf = [ Pratt.literal digits ]
        , andThenOneOf = []
        , spaces = Parser.succeed ()
        }
        |> node


node : Parser a -> Parser (Node a)
node parser =
    Parser.succeed (\start a end -> Node { start = toLocation start, end = toLocation end } a)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


toLocation : ( Int, Int ) -> Location
toLocation ( line, column ) =
    { row = line, column = column }


digits : Parser Expression
digits =
    Parser.number
        { int = Just Expression.IntegerLiteral
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Nothing
        }
