module ParserFast.Extra exposing (anyChar)

import ParserFast


anyChar : ParserFast.Parser Char
anyChar =
    ParserFast.chompAnyChar
        |> ParserFast.getChompedString
        |> ParserFast.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        problemAnyCharacter

                    c :: _ ->
                        ParserFast.succeed c
            )


problemAnyCharacter : ParserFast.Parser a
problemAnyCharacter =
    ParserFast.problem "expected any character"
