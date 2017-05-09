module Elm.Parser.File exposing (file)

import Combine exposing (maybe, (*>), (<*), sepBy, succeed, Parser, (<*>), withState)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Declarations exposing (declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Parser.Util exposing (exactIndentWhitespace)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Range exposing (Range)


file : Parser State File
file =
    succeed File
        <*> (maybe exactIndentWhitespace *> moduleDefinition <* maybe exactIndentWhitespace)
        <*> (sepBy exactIndentWhitespace importDefinition <* maybe exactIndentWhitespace)
        <*> fileDeclarations
        <*> collectComments


collectComments : Parser State (List ( String, Range ))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List Declaration)
fileDeclarations =
    sepBy exactIndentWhitespace declaration <* maybe exactIndentWhitespace <* manySpaces
