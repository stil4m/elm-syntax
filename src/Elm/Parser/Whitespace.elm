module Elm.Parser.Whitespace exposing (many1Spaces, manySpaces, nSpaces, realNewLine, untilNewlineToken)

import Combine exposing (Parser, regex)


nSpaces : Int -> Parser s String
nSpaces x =
    regex (" {" ++ toString x ++ "}")


manySpaces : Parser s String
manySpaces =
    regex " *"


many1Spaces : Parser s String
many1Spaces =
    regex " +"


realNewLine : Parser s String
realNewLine =
    regex "\u{000D}?\n"


untilNewlineToken : Parser s String
untilNewlineToken =
    regex "[^\u{000D}\n]*"
