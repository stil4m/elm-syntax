module Parser.Extra exposing (anyChar, continueWith, withIndent)

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


{-| Like `Core.andThen (\() -> ...)` but circumvents laziness
-}
continueWith : Core.Parser b -> Core.Parser () -> Core.Parser b
continueWith b a =
    a |> Core.andThen (\() -> b)


{-| For a given ParserWithComments.Parser, take the current start column as indentation for the whole block
-}
withIndent : Core.Parser a -> Core.Parser a
withIndent p =
    Core.andThen (\column -> Core.withIndent column p)
        Core.getCol
