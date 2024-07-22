module Parser.Extra exposing (anyChar, location)

import Elm.Syntax.Range exposing (Location)
import Parser as Core


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
