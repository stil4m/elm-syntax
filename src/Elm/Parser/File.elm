module Elm.Parser.File exposing (file)

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Declarations exposing (declaration)
import Elm.Parser.Imports exposing (importDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules exposing (moduleDefinition)
import Elm.Parser.Node as Node
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)


file : Parser State File
file =
    Combine.maybeIgnore Layout.layoutStrict
        |> Combine.continueWith
            (Node.parserMap
                (\moduleDefinition ->
                    \imports ->
                        \declarations ->
                            \comments ->
                                { moduleDefinition = moduleDefinition
                                , imports = imports
                                , declarations = declarations
                                , comments = comments
                                }
                )
                moduleDefinition
            )
        |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
        |> Combine.ignore (Combine.maybeIgnore (Comments.moduleDocumentation |> Combine.ignore Layout.layoutStrict))
        |> Combine.keep (Combine.many importDefinition)
        |> Combine.keep fileDeclarations
        |> Combine.keep collectComments


collectComments : Parser State (List (Node String))
collectComments =
    Combine.withState (\state -> Combine.succeed (State.getComments state))


fileDeclarations : Parser State (List (Node Declaration))
fileDeclarations =
    Combine.many
        (declaration
            |> Combine.ignore (Combine.maybeIgnore Layout.layoutStrict)
        )
