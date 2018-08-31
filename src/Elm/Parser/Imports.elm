module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser, maybe, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


importDefinition : Parser State Import
importDefinition =
    let
        importAndModuleName =
            importToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)

        asDefinition : Parser State (Node ModuleName)
        asDefinition =
            asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)

        parseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
        parseExposingDefinition mod asDef =
            Combine.choice
                [ exposeDefinition
                    |> Combine.map (Just >> Import mod asDef)
                , Combine.succeed (Import mod asDef Nothing)
                ]

        parseAsDefinition mod =
            Combine.choice
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (Just >> parseExposingDefinition mod)
                , parseExposingDefinition mod Nothing
                ]
    in
    importAndModuleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen parseAsDefinition
