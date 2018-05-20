module Elm.Parser.File exposing (file)

import Combine exposing (Parser, many, maybe, sepBy, succeed, withState)
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
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap moduleDefinition
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap (many (importDefinition |> Combine.ignore Layout.optimisticLayout))
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap fileDeclarations
        |> Combine.andMap collectComments
        |> Combine.ignore Layout.optimisticLayout


collectComments : Parser State (List (Ranged String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List (Ranged Declaration))
fileDeclarations =
    many
        (declaration
            |> Combine.ignore (maybe Layout.layoutStrict)
        )
