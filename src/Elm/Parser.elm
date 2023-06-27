module Elm.Parser exposing (parse)

{-|

@docs parse

-}

import Combine exposing (Parser, end, withLocation)
import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser.File exposing (file)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Processing as Processing
import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)


{-| Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
-}
parse : String -> Result (List DeadEnd) File
parse input =
    -- A single line is added for unfinished ranges produced by `parser-combinators` on the last line.
    case Combine.runParser (withEnd file) emptyState (input ++ "\n") of
        Ok ( _, r ) ->
            Ok (Processing.process (InternalRawFile.fromFile r))

        Err s ->
            Err s


withEnd : Parser State File -> Parser State File
withEnd p =
    p |> Combine.ignore (withLocation (\_ -> end))
