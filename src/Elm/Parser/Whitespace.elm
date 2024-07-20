module Elm.Parser.Whitespace exposing (many1Spaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.))


many1Spaces : Parser state ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Core.Parser ()
realNewLine =
    Core.oneOf
        [ Core.symbol "\u{000D}\n"
        , Core.symbol "\n"
        ]


untilNewlineToken : Core.Parser ()
untilNewlineToken =
    Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
