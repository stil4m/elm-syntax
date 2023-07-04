module Elm.Parser.ExpressionV2 exposing (expression)

import Elm.Syntax.Expression as Expression exposing (Expression(..), StringLiteralType(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Hex
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Pratt.Advanced as Pratt


type Problem
    = P
    | Explanation String


expression : Parser c Problem (Node Expression)
expression =
    Pratt.expression
        { oneOf =
            [ Pratt.literal digits
            , stringLiteral
                |> Parser.map (\s -> StringLiteral SingleQuote s)
                |> Pratt.literal
            , quotedSingleQuote
                |> Parser.map CharLiteral
                |> Pratt.literal
            ]
        , andThenOneOf = []
        , spaces = Parser.succeed ()
        }
        |> node


stringLiteral : Parser c Problem String
stringLiteral =
    let
        helper s =
            if s.escaped then
                escapedCharValue
                    |> Parser.map
                        (\v ->
                            Parser.Loop { escaped = False, parts = String.fromChar v :: s.parts }
                        )

            else
                Parser.oneOf
                    [ Parser.symbol (Parser.Token "\"" P)
                        |> Parser.map (\_ -> Parser.Done (String.concat <| List.reverse s.parts))
                    , Parser.getChompedString (Parser.symbol (Parser.Token "\\" P))
                        |> Parser.map (\_ -> Parser.Loop { escaped = True, parts = s.parts })
                    , Parser.succeed (\start value end -> ( start, value, end ))
                        |= Parser.getOffset
                        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '"' && c /= '\\'))
                        |= Parser.getOffset
                        |> Parser.andThen
                            (\( start, value, end ) ->
                                if start == end then
                                    Parser.problem (Explanation "Expected a string character or a double quote")

                                else
                                    Parser.succeed (Parser.Loop { escaped = s.escaped, parts = value :: s.parts })
                            )
                    ]
    in
    Parser.succeed identity
        |. Parser.symbol (Parser.Token "\"" P)
        |= Parser.loop { escaped = False, parts = [] } helper


quotedSingleQuote : Parser c Problem Char
quotedSingleQuote =
    Parser.succeed (String.toList >> List.head >> Maybe.withDefault ' ')
        |. Parser.symbol (Parser.Token "'" P)
        |= Parser.oneOf
            [ Parser.succeed (List.singleton >> String.fromList)
                |. Parser.symbol (Parser.Token "\\" P)
                |= escapedCharValue
            , Parser.getChompedString (Parser.chompIf (always True) P)
            ]
        |. Parser.symbol (Parser.Token "'" P)


escapedCharValue : Parser c Problem Char
escapedCharValue =
    Parser.oneOf
        [ Parser.succeed '\'' |. Parser.symbol (Parser.Token "'" P)
        , Parser.succeed '"' |. Parser.symbol (Parser.Token "\"" P)
        , Parser.succeed '\n' |. Parser.symbol (Parser.Token "n" P)
        , Parser.succeed '\t' |. Parser.symbol (Parser.Token "t" P)
        , -- Even though elm-format will change \r to a unicode version. When you don't use elm-format, this will not happen.
          Parser.succeed '\u{000D}' |. Parser.symbol (Parser.Token "r" P)
        , Parser.succeed '\\' |. Parser.symbol (Parser.Token "\\" P)
        , Parser.succeed (String.toLower >> Hex.fromString >> Result.withDefault 0 >> Char.fromCode)
            |. Parser.symbol (Parser.Token "u" P)
            |. Parser.symbol (Parser.Token "{" P)
            |= (Parser.chompWhile (\c -> String.any ((==) c) "0123456789ABCDEFabcdef") |> Parser.getChompedString)
            |. Parser.symbol (Parser.Token "}" P)
        ]



--string =
--    Parser.succeed identity
--        |. Parser.symbol (Parser.Token "\"" ())
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


digits : Parser c Problem Expression
digits =
    Parser.number
        { int = Ok Expression.IntegerLiteral
        , hex = Err P
        , octal = Err P
        , binary = Err P
        , float = Err P
        , invalid = P
        , expecting = P
        }


node : Parser c x a -> Parser c x (Node a)
node parser =
    Parser.succeed (\start a end -> Node { start = toLocation start, end = toLocation end } a)
        |= Parser.getPosition
        |= parser
        |= Parser.getPosition


toLocation : ( Int, Int ) -> Location
toLocation ( line, column ) =
    { row = line, column = column }
