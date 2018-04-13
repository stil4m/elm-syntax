module Elm.Parser.File exposing (file)

import Combine exposing ((*>), (<*), (<*>), Parser, maybe, sepBy, succeed, withState)
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Ranges exposing (ranged)
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Ranged exposing (Ranged)


file : Parser State File
file =
    succeed File
        <*> (maybe Layout.layoutAndNewLine *> moduleDefinition <* maybe Layout.layoutAndNewLine)
        <*> (sepBy Layout.layoutAndNewLine importDefinition <* maybe Layout.layoutAndNewLine)
        <*> fileDeclarations
        <*> collectComments


collectComments : Parser State (List (Ranged String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List (Ranged Declaration))
fileDeclarations =
    sepBy Layout.layoutAndNewLine (ranged declaration) <* maybe Layout.layout <* maybe Layout.layoutAndNewLine
