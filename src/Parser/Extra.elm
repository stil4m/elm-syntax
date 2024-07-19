module Parser.Extra exposing (anyChar, location, many1Ignore, sepBy1)

import Elm.Syntax.Range exposing (Location)
import Parser as Core exposing ((|.), (|=))


location : Core.Parser Location
location =
    Core.getPosition
        |> Core.map
            (\( row, col ) ->
                { row = row, column = col }
            )


anyChar : Core.Parser Char
anyChar =
    Core.chompIf (always True)
        |> Core.getChompedString
        |> Core.andThen
            (\s ->
                case String.toList s of
                    [] ->
                        Core.problem "expected any character"

                    c :: _ ->
                        Core.succeed c
            )


sepBy1 : String -> Core.Parser a -> Core.Parser (List a)
sepBy1 sep p =
    Core.succeed cons
        |= p
        |= many
            (Core.succeed identity
                |. Core.symbol sep
                |= p
            )


many : Core.Parser a -> Core.Parser (List a)
many p =
    manyWithoutReverse [] p
        |> Core.map List.reverse


manyWithoutReverse : List a -> Core.Parser a -> Core.Parser (List a)
manyWithoutReverse initList p =
    let
        helper : List a -> Core.Parser (Core.Step (List a) (List a))
        helper items =
            Core.oneOf
                [ p
                    |> Core.map (\item -> Core.Loop (item :: items))
                , Core.succeed (Core.Done items)
                ]
    in
    Core.loop initList helper


cons : a -> List a -> List a
cons first =
    \rest -> first :: rest


many1Ignore : Core.Parser () -> Core.Parser ()
many1Ignore p =
    p
        |. manyIgnore p


manyIgnore : Core.Parser () -> Core.Parser ()
manyIgnore p =
    let
        helper : () -> Core.Parser (Core.Step () ())
        helper () =
            Core.oneOf
                [ p
                    |> Core.map Core.Loop
                , Core.succeed (Core.Done ())
                ]
    in
    Core.loop () helper
