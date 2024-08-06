module Elm.Parser.TestUtil exposing (parse, parseToResult)

import Parser
import ParserFast


parse : String -> ParserFast.Parser a -> Maybe a
parse source p =
    parseToResult source p |> Result.toMaybe


parseToResult : String -> ParserFast.Parser a -> Result (List Parser.DeadEnd) a
parseToResult source p =
    ParserFast.run (p |> ParserFast.ignore ParserFast.end) source
