module Elm.Parser.Whitespace exposing (many1Spaces, manySpaces, nSpaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.), (|=), Step(..))


nSpaces : Int -> Parser s String
nSpaces x =
    let
        helper : Int -> Core.Parser (Step Int String)
        helper n =
            if n == 0 then
                Core.succeed (Done (String.repeat x " "))

            else
                Core.succeed (\_ -> Loop (n - 1))
                    |= Core.token " "
    in
    Core.loop x helper
        |> Combine.fromCore


manySpaces : Parser s ()
manySpaces =
    Combine.fromCore (Core.chompWhile (\c -> c == ' '))


many1Spaces : Parser s ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Parser s String
realNewLine =
    Core.getChompedString
        (Core.succeed ()
            |. Core.oneOf [ Core.chompIf ((==) '\u{000D}'), Core.succeed () ]
            |. Core.symbol "\n"
        )
        |> Combine.fromCore


untilNewlineToken : Parser s String
untilNewlineToken =
    Core.getChompedString (Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n'))
        |> Combine.fromCore
