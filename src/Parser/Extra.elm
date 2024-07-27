module Parser.Extra exposing (anyChar, continueWith, withIndent)

import Parser


anyChar : Parser.Parser Char
anyChar =
    Parser.chompIf (always True)
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        problemAnyCharacter

                    c :: _ ->
                        Parser.succeed c
            )


problemAnyCharacter : Parser.Parser a
problemAnyCharacter =
    Parser.problem "expected any character"


{-| Like `Parser.andThen (\() -> ...)` but circumvents laziness
-}
continueWith : Parser.Parser b -> Parser.Parser () -> Parser.Parser b
continueWith b a =
    a |> Parser.andThen (\() -> b)


{-| For a given ParserWithComments.Parser, take the current start column as indentation for the whole block
-}
withIndent : Parser.Parser a -> Parser.Parser a
withIndent p =
    Parser.andThen (\column -> Parser.withIndent column p)
        Parser.getCol
