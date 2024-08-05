module CustomParser.Extra exposing (anyChar)

import CustomParser


anyChar : CustomParser.Parser Char
anyChar =
    CustomParser.chompIf (always True)
        |> CustomParser.getChompedString
        |> CustomParser.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        problemAnyCharacter

                    c :: _ ->
                        CustomParser.succeed c
            )


problemAnyCharacter : CustomParser.Parser a
problemAnyCharacter =
    CustomParser.problem "expected any character"
