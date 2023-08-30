module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, importToken)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)


importDefinition : Parser State (Node Import)
importDefinition =
    let
        asDefinition : Parser State (Node ModuleName)
        asDefinition =
            asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)

        parseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
        parseExposingDefinition mod asDef =
            Combine.oneOf
                [ Node.parser exposeDefinition
                    |> Combine.map Just
                , Combine.succeed Nothing
                ]
                |> Combine.map (\exposing_ -> Import mod asDef exposing_)

        parseAsDefinition : Node ModuleName -> Parser State Import
        parseAsDefinition mod =
            Combine.oneOf
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (\alias_ -> parseExposingDefinition mod (Just alias_))
                , parseExposingDefinition mod Nothing
                ]
    in
    Combine.withLocation
        (\start ->
            importToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Node.parser moduleName)
                |> Combine.ignore Layout.optimisticLayout
                |> Combine.andThen parseAsDefinition
                |> Combine.map (setupNode start)
        )
        |> Combine.ignore Layout.optimisticLayout


setupNode : Location -> Import -> Node Import
setupNode start imp =
    let
        endRange : Range
        endRange =
            case imp.moduleAlias of
                Just moduleAlias ->
                    Node.range moduleAlias

                Nothing ->
                    case imp.exposingList of
                        Just exposingList ->
                            Node.range exposingList

                        Nothing ->
                            Node.range imp.moduleName
    in
    Node
        { start = start, end = endRange.end }
        imp
