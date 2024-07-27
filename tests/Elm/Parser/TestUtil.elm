module Elm.Parser.TestUtil exposing (parse)

import Parser exposing ((|.))


parse : String -> Parser.Parser a -> Maybe a
parse source p =
    Parser.run (p |. Parser.end) source
        |> Result.toMaybe
