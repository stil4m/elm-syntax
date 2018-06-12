module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser, maybe, succeed)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposable, exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Module exposing (Import)


importDefinition : Parser State Import
importDefinition =
    let
        importAndModuleName =
            importToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleName

        asDefinition =
            asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith moduleName

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
    withRange <|
        (importAndModuleName
            |> Combine.ignore Layout.optimisticLayout
            |> Combine.andThen parseAsDefinition
        )
