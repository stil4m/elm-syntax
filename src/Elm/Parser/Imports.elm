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
import Elm.Syntax.Range as Range exposing (Range)


importDefinition : Parser State (Node Import)
importDefinition =
    let
        importAndModuleName : Parser State (Node ModuleName)
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
            Combine.oneOf
                [ Node.parser exposeDefinition
                    |> Combine.map (Just >> Import mod asDef)
                , Combine.succeed (Import mod asDef Nothing)
                ]

        parseAsDefinition : Node ModuleName -> Parser State Import
        parseAsDefinition mod =
            Combine.oneOf
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (Just >> parseExposingDefinition mod)
                , parseExposingDefinition mod Nothing
                ]
    in
    Node.parser (Combine.succeed ())
        |> Combine.andThen
            (\(Node start ()) ->
                importAndModuleName
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen parseAsDefinition
                    |> Combine.map (setupNode start)
            )
        |> Combine.ignore Layout.optimisticLayout


setupNode : Range -> Import -> Node Import
setupNode start imp =
    let
        allRanges : List (Maybe Range)
        allRanges =
            [ Just start
            , Just (Node.range imp.moduleName)
            , Maybe.map Node.range imp.exposingList
            , Maybe.map Node.range imp.moduleAlias
            ]
    in
    Node
        (Range.combine (List.filterMap identity allRanges))
        imp
