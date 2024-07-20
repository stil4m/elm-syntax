module Parser.Extra exposing (anyChar, location, many1Ignore)

import Elm.Syntax.Range exposing (Location)
import Parser as Core exposing ((|.))


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
