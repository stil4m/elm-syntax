module Combine.Char exposing (anyChar, char, oneOf)

import Combine exposing (Parser, primitive)


char : Char -> Parser s Char
char c =
    satisfy ((==) c)
        |> Combine.setError ("expected " ++ Debug.toString c)


anyChar : Parser s Char
anyChar =
    satisfy (always True)
        |> Combine.setError "expected any character"


oneOf : List Char -> Parser s Char
oneOf cs =
    satisfy (\a -> List.member a cs)
        |> Combine.setError ("expected one of " ++ Debug.toString cs)


satisfy : (Char -> Bool) -> Parser s Char
satisfy pred =
    primitive <|
        \state stream ->
            let
                message =
                    "could not satisfy predicate"
            in
            case String.uncons stream.input of
                Just ( h, rest ) ->
                    if pred h then
                        ( state, { stream | input = rest, position = stream.position + 1 }, Ok h )

                    else
                        ( state, stream, Err [ message ] )

                Nothing ->
                    ( state, stream, Err [ message ] )
