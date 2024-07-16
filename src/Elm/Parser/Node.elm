module Elm.Parser.Node exposing (parser, parserCore, parserFromCore)

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))
import Parser.Extra


parser : Parser state a -> Parser state (Node a)
parser p =
    Combine.succeed (\start -> \v -> \end -> Node { start = start, end = end } v)
        |> Combine.keepFromCore Parser.Extra.location
        |> Combine.keep p
        |> Combine.keepFromCore Parser.Extra.location


parserFromCore : Core.Parser a -> Parser state (Node a)
parserFromCore p =
    parserCore p
        |> Combine.fromCore


parserCore : Core.Parser a -> Core.Parser (Node a)
parserCore p =
    Core.succeed (\start -> \v -> \end -> Node { start = start, end = end } v)
        |= Parser.Extra.location
        |= p
        |= Parser.Extra.location
