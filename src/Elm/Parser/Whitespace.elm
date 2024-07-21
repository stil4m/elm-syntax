module Elm.Parser.Whitespace exposing (many1Spaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser)
import Parser as Core
import Set


many1Spaces : Parser state ()
many1Spaces =
    -- yes, this is faster than  symbol/chompIf |. chompWhile
    Core.variable
        { start = \c -> c == ' '
        , inner = \c -> c == ' '
        , reserved = Set.empty
        }
        |> Core.map (\_ -> ())
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
