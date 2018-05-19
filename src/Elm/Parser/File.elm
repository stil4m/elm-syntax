module Elm.Parser.File exposing (file)

import Combine exposing (Parser, maybe, sepBy, succeed, withState)
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
        |> Combine.andMap (maybe Layout.layoutAndNewLine |> Combine.continueWith moduleDefinition |> Combine.ignore (maybe Layout.layoutAndNewLine))
        |> Combine.andMap (sepBy Layout.layoutAndNewLine importDefinition |> Combine.ignore (maybe Layout.layoutAndNewLine))
        |> Combine.andMap fileDeclarations
        |> Combine.andMap collectComments


collectComments : Parser State (List (Ranged String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List (Ranged Declaration))
fileDeclarations =
    sepBy Layout.layoutAndNewLine (ranged declaration)
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.ignore (maybe Layout.layoutAndNewLine)
