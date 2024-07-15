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
                |> Combine.continueWith moduleName

        parseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
        parseExposingDefinition mod asDef =
            Combine.oneOf
                [ Node.parser exposeDefinition
                    |> Combine.map Just
                , Combine.succeed Nothing
                ]
                |> Combine.map (\exposing_ -> Import mod asDef exposing_)

        parseAsDefinition : Location -> Node ModuleName -> Parser State (Node Import)
        parseAsDefinition start mod =
            Combine.oneOf
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (\alias_ -> parseExposingDefinition mod (Just alias_))
                , parseExposingDefinition mod Nothing
                ]
                |> Combine.map (\imp -> setupNode start imp)
    in
    Combine.succeed (\start -> \mod -> parseAsDefinition start mod)
        |> Combine.keep Combine.location
        |> Combine.ignore importToken
        |> Combine.ignore Layout.layout
        |> Combine.keep moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen identity
        |> Combine.ignore Layout.optimisticLayout


setupNode : Location -> Import -> Node Import
setupNode start imp =
    let
        endRange : Range
        endRange =
            case imp.moduleAlias of
                Just (Node range _) ->
                    range

                Nothing ->
                    case imp.exposingList of
                        Just (Node range _) ->
                            range

                        Nothing ->
                            Node.range imp.moduleName
    in
    Node
        { start = start, end = endRange.end }
        imp
