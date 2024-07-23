module Parser.Extra exposing (anyChar)

import Parser as Core


anyChar : Core.Parser Char
anyChar =
    Core.chompIf (always True)
        |> Core.getChompedString
        |> Core.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        problemAnyCharacter

                    c :: _ ->
                        Core.succeed c
            )


problemAnyCharacter : Core.Parser a
problemAnyCharacter =
    Core.problem "expected any character"
