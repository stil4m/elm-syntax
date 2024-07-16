module Combine.Char exposing (anyChar, char)

import Parser as Core


char : Char -> Core.Parser Char
char c =
    satisfy
        (\c_ -> c_ == c)
        ("expected '" ++ String.fromChar c ++ "'")


anyChar : Core.Parser Char
anyChar =
    satisfy
        (always True)
        "expected any character"


satisfy : (Char -> Bool) -> String -> Core.Parser Char
satisfy pred problem =
    Core.chompIf pred
        |> Core.getChompedString
        |> Core.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        Core.problem problem

                    c :: _ ->
                        Core.succeed c
            )
