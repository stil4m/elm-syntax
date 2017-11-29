module Elm.Parser.File exposing (file)

import Combine exposing ((*>), (<*), (<*>), Parser, maybe, sepBy, succeed, withState)
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Util exposing (exactIndentWhitespace)
import Elm.Parser.Whitespace exposing (manySpaces)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Ranged exposing (Ranged)


file : Parser State File
file =
    succeed File
        <*> (maybe exactIndentWhitespace *> moduleDefinition <* maybe exactIndentWhitespace)
        <*> (sepBy exactIndentWhitespace importDefinition <* maybe exactIndentWhitespace)
        <*> fileDeclarations
        <*> collectComments


collectComments : Parser State (List (Ranged String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List Declaration)
fileDeclarations =
    sepBy exactIndentWhitespace declaration <* maybe exactIndentWhitespace <* manySpaces
