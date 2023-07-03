module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Expression as Expression exposing (Expression(..), StringLiteralType(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Parser exposing ((|.), (|=), Parser)
import Pratt


expression : Parser (Node Expression)
expression =
    Pratt.expression
        { oneOf =
            [ Pratt.literal digits
            , Tokens.stringLiteral
                |> Parser.map (\s -> StringLiteral SingleQuote s)
                |> Pratt.literal
            , Tokens.quotedSingleQuote
                |> Parser.map CharLiteral
                |> Pratt.literal
            ]
        , andThenOneOf = []
        , spaces = Parser.succeed ()
        }
        |> node



--string =
--    Parser.succeed identity
--        |. Parser.symbol "\""
--        --|= Parser.oneOf
--        --    [ Parser.map (\str -> StringLiteral TripleQuote str) multiLineStringLiteral
--        --    , Parser.map (\str -> StringLiteral SingleQuote str) stringLiteral
--        --    ]
--        |= Parser.map (\str -> StringLiteral SingleQuote str) stringLiteral
--        |> node


type alias StringLiteralLoopState =
    { escaped : Bool
    , parts : List String
    }


digits : Parser Expression
digits =
    Parser.number
        { int = Just Expression.IntegerLiteral
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Nothing
        }


node : Parser a -> Parser (Node a)
node parser =
    Parser.succeed (\start a end -> Node { start = toLocation start, end = toLocation end } a)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


toLocation : ( Int, Int ) -> Location
toLocation ( line, column ) =
    { row = line, column = column }
