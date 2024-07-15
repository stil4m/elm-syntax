module Elm.Parser.Node exposing (parser, parserCore, parserFromCore)

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))


parser : Parser state a -> Parser state (Node a)
parser p =
    Combine.succeed (\start -> \v -> \end -> Node { start = start, end = end } v)
        |> Combine.keep Combine.location
        |> Combine.keep p
        |> Combine.keep Combine.location


parserFromCore : Core.Parser a -> Parser state (Node a)
parserFromCore p =
    parserCore p
        |> Combine.fromCore


parserCore : Core.Parser a -> Core.Parser (Node a)
parserCore p =
    Core.succeed (\( s_row, s_col ) -> \v -> \( e_row, e_col ) -> Node { start = { row = s_row, column = s_col }, end = { row = e_row, column = e_col } } v)
        |= Core.getPosition
        |= p
        |= Core.getPosition
