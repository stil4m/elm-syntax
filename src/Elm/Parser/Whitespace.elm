module Elm.Parser.Whitespace exposing (many1Spaces, realNewLine, realNewLineCore, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core exposing ((|.))


many1Spaces : Parser s ()
many1Spaces =
    Core.token " "
        |. Core.chompWhile (\c -> c == ' ')
        |> Combine.fromCore


realNewLine : Parser state ()
realNewLine =
    Combine.fromCore realNewLineCore


realNewLineCore : Core.Parser ()
realNewLineCore =
    Core.oneOf
        [ Core.chompIf (\c -> c == '\u{000D}')
        , Core.succeed ()
        ]
        |. Core.symbol "\n"


untilNewlineToken : Core.Parser ()
untilNewlineToken =
    Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
