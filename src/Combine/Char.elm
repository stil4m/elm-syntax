module Combine.Char exposing (anyChar, char)

import Combine exposing (Parser)
import Parser as Core


char : Char -> Parser state Char
char c =
    satisfy
        (\c_ -> c_ == c)
        ("expected '" ++ String.fromChar c ++ "'")
        |> Combine.fromCore


anyChar : Parser s Char
anyChar =
    satisfy
        (always True)
        "expected any character"
        |> Combine.fromCore


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
