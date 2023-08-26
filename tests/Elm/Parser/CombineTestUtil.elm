module Elm.Parser.CombineTestUtil exposing (parse, parseWithState)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State, emptyState)


parseWithState : String -> Parser State a -> Maybe ( State, a )
parseWithState s p =
    Combine.runParser (p |> Combine.ignore Combine.end) emptyState s
        |> Result.toMaybe


parse : String -> Parser State a -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map Tuple.second
