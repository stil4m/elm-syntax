module Elm.Parser exposing (parse)

{-|

@docs parse

-}

import Combine exposing (Parser, end)
import Elm.Parser.File exposing (file)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)


{-| Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
-}
parse : String -> Result (List DeadEnd) File
parse input =
    case Combine.runParser (withEnd file) emptyState input of
        Ok ( _, fileContents ) ->
            Ok fileContents

        Err s ->
            Err s


withEnd : Parser State File -> Parser State File
withEnd p =
    p |> Combine.ignore end
