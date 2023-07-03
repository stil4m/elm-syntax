module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Syntax.Expression as Expression exposing (Expression(..), StringLiteralType(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Hex
import Parser exposing ((|.), (|=), Parser)
import Pratt


expression : Parser (Node Expression)
expression =
    Pratt.expression
        { oneOf =
            [ Pratt.literal digits
            , stringLiteral
                |> Parser.map (\s -> StringLiteral SingleQuote s)
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


stringLiteral : Parser String
stringLiteral =
    let
        helper : StringLiteralLoopState -> Parser.Parser (Parser.Step StringLiteralLoopState String)
        helper { escaped, parts } =
            if escaped then
                escapedCharValue
                    |> Parser.map
                        (\v ->
                            Parser.Loop { escaped = False, parts = String.fromChar v :: parts }
                        )

            else
                Parser.oneOf
                    [ Parser.symbol "\""
                        |> Parser.map (\_ -> Parser.Done (String.concat <| List.reverse parts))
                    , Parser.getChompedString (Parser.symbol "\\")
                        |> Parser.map (\_ -> Parser.Loop { escaped = True, parts = parts })
                    , Parser.succeed (\start value end -> ( start, value, end ))
                        |= Parser.getOffset
                        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Parser.getOffset
                        |> Parser.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Parser.problem "Expected a string character or a double quote"

                                else
                                    Parser.succeed (Parser.Loop { escaped = escaped, parts = value :: parts })
                            )
                    ]
    in
    Parser.succeed identity
        |. Parser.symbol "\""
        |= Parser.loop { escaped = False, parts = [] } helper


escapedCharValue : Parser Char
escapedCharValue =
    Parser.oneOf
        [ Parser.succeed '\'' |. Parser.symbol "'"
        , Parser.succeed '"' |. Parser.symbol "\""
        , Parser.succeed '\n' |. Parser.symbol "n"
        , Parser.succeed '\t' |. Parser.symbol "t"
        , -- Eventhough Elm-format will change \r to a unicode version. When you dont use elm-format, this will not happen.
          Parser.succeed '\u{000D}' |. Parser.symbol "r"
        , Parser.succeed '\\' |. Parser.symbol "\\"
        , Parser.succeed (String.toLower >> Hex.fromString >> Result.withDefault 0 >> Char.fromCode)
            |. Parser.symbol "u"
            |. Parser.symbol "{"
            |= (Parser.chompWhile (\c -> String.any ((==) c) "0123456789ABCDEFabcdef") |> Parser.getChompedString)
            |. Parser.symbol "}"
        ]


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
