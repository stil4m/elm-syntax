module Elm.Parser exposing (parse)

{-|


# Parsing Syntax

@docs parse

-}

import Elm.Syntax.File exposing (File)
import Combine exposing (Parser, (<*), end, mapError, withLocation)
import Elm.Parser.File exposing (file)
import Elm.Parser.State exposing (State, emptyState)


{-| Parse a string
-}
parse : String -> Result (List String) File
parse input =
    -- A single line is added for unfinished ranges produced by `parser-combinators` on the last line.
    case Combine.runParser (withEnd file) emptyState (input ++ "\n") of
        Ok ( _, _, r ) ->
            Ok r

        Err ( _, _, s ) ->
            Err s


withEnd : Parser State File -> Parser State File
withEnd p =
    p <* withLocation (\s -> end |> mapError (\_ -> [ "Could not continue parsing on location " ++ toString ( s.line, s.column ) ]))
