module Combine.Char exposing (anyChar, char, oneOf)

import Combine exposing (Parser)
import Parser as Core


char : Char -> Parser s Char
char c =
    satisfy ((==) c)
        |> Combine.andThen (Maybe.map Combine.succeed >> Maybe.withDefault (Combine.fail ("expected '" ++ String.fromList [ c ] ++ "'")))


anyChar : Parser s Char
anyChar =
    satisfy (always True)
        |> Combine.andThen (Maybe.map Combine.succeed >> Maybe.withDefault (Combine.fail "expected any character"))


oneOf : List Char -> Parser s Char
oneOf cs =
    satisfy (\a -> List.member a cs)
        |> Combine.andThen (Maybe.map Combine.succeed >> Maybe.withDefault (Combine.fail ("expected one of '" ++ String.fromList cs ++ "'")))


satisfy : (Char -> Bool) -> Parser s (Maybe Char)
satisfy pred =
    Combine.fromCore
        (Core.getChompedString (Core.chompIf pred)
            |> Core.andThen
                (\s ->
                    case String.toList s of
                        [] ->
                            Core.succeed Nothing

                        c :: _ ->
                            Core.succeed (Just c)
                )
        )
