module Elm.Parser.TestUtil exposing (parse, parseToResult)

import CustomParser


parse : String -> CustomParser.Parser a -> Maybe a
parse source p =
    parseToResult source p |> Result.toMaybe


parseToResult : String -> CustomParser.Parser a -> Result (List CustomParser.DeadEnd) a
parseToResult source p =
    CustomParser.run (p |> CustomParser.ignore CustomParser.end) source
