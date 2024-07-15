module Elm.Parser.Whitespace exposing (many1Spaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.))


many1Spaces : Parser s ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Parser s ()
realNewLine =
    Core.oneOf
        [ Core.chompIf (\c -> c == '\u{000D}')
        , Core.succeed ()
        ]
        |. Core.symbol "\n"
        |> Combine.fromCore


untilNewlineToken : Core.Parser ()
untilNewlineToken =
    Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
