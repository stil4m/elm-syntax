module Elm.Parser.Node exposing (parser)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Node exposing (Node(..))


parser : Parser State a -> Parser State (Node a)
parser p =
    Combine.succeed (\start -> \v -> \end -> Node { start = start, end = end } v)
        |> Combine.keep Combine.location
        |> Combine.keep p
        |> Combine.keep Combine.location
