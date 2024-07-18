module Elm.Parser exposing (parse)

{-|

@docs parse

-}

import Elm.Parser.File exposing (file)
import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)


{-| Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
-}
parse : String -> Result (List DeadEnd) File
parse input =
    Parser.run file input
