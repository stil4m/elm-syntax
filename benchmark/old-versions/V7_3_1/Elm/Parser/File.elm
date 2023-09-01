module V7_3_1.Elm.Parser.File exposing (file)

import V7_3_1.Combine as Combine exposing (Parser, many, maybe, succeed, withState)
import V7_3_1.Elm.Parser.Declarations exposing (declaration)
import V7_3_1.Elm.Parser.Imports exposing (importDefinition)
import V7_3_1.Elm.Parser.Layout as Layout
import V7_3_1.Elm.Parser.Modules exposing (moduleDefinition)
import V7_3_1.Elm.Parser.Node as Node
import V7_3_1.Elm.Parser.State as State exposing (State)
import V7_3_1.Elm.Syntax.Declaration exposing (Declaration)
import V7_3_1.Elm.Syntax.File exposing (File)
import V7_3_1.Elm.Syntax.Node exposing (Node)


file : Parser State File
file =
    succeed File
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap (Node.parser moduleDefinition)
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap (many (importDefinition |> Combine.ignore Layout.optimisticLayout))
        |> Combine.ignore (maybe Layout.layoutStrict)
        |> Combine.andMap fileDeclarations
        |> Combine.andMap collectComments
        |> Combine.ignore Layout.optimisticLayout


collectComments : Parser State (List (Node String))
collectComments =
    withState (State.getComments >> succeed)


fileDeclarations : Parser State (List (Node Declaration))
fileDeclarations =
    many
        (declaration
            |> Combine.ignore (maybe Layout.layoutStrict)
        )
