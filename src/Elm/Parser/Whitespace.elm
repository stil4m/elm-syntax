module Elm.Parser.Whitespace exposing (untilNewlineToken)

import Parser as Core


untilNewlineToken : Core.Parser ()
untilNewlineToken =
    Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
