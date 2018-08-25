module Elm.Parser exposing (parse)

{-|


# Parsing Syntax

@docs parse

-}

import Combine exposing (Parser, end, withLocation)
import Elm.Internal.RawFile as RawFile exposing (RawFile)
import Elm.Parser.File exposing (file)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Syntax.File exposing (File)
import Platform


{-| Parse a string
-}
parse : String -> Result (List String) RawFile
parse input =
    -- A single line is added for unfinished ranges produced by `parser-combinators` on the last line.
    case Combine.runParser (withEnd file) emptyState (input ++ "\n") of
        Ok ( _, r ) ->
            Ok (RawFile.fromFile r)

        Err s ->
            Err [ "Some error" ]


withEnd : Parser State File -> Parser State File
withEnd p =
    p |> Combine.ignore (withLocation (\s -> end))


formatLocation : Combine.ParseLocation -> String
formatLocation { line, column } =
    String.concat
        [ "("
        , String.fromInt line
        , ","
        , String.fromInt column
        , ")"
        ]
