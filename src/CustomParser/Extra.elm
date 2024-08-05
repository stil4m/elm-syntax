module CustomParser.Extra exposing (anyChar, withIndent)

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


{-| For a given ParserWithComments.Parser, take the current start column as indentation for the whole block
-}
withIndent : CustomParser.Parser a -> CustomParser.Parser a
withIndent p =
    CustomParser.columnAndThen (\column -> CustomParser.withIndent column p)
