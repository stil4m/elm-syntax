module Elm.Parser.TestUtil exposing (parse)

import Parser as Core exposing ((|.))


parse : String -> Core.Parser a -> Maybe a
parse source p =
    Core.run (p |. Core.end) source
        |> Result.toMaybe
