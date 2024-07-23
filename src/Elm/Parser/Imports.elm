module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser as Core


importDefinition : Parser State (Node Import)
importDefinition =
    Combine.succeed
        (\( startRow, startColumn ) ->
            \mod ->
                importInnerParseAsDefinition { row = startRow, column = startColumn } mod
        )
        |> Combine.keepFromCore Core.getPosition
        |> Combine.ignoreEntirely Tokens.importToken
        |> Combine.ignore Layout.layout
        |> Combine.keepFromCore moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen identity
        |> Combine.ignore Layout.optimisticLayout


importInnerAsDefinitionOptimisticLayout : Parser State (Node ModuleName)
importInnerAsDefinitionOptimisticLayout =
    Tokens.asToken
        |> Combine.ignoreFromCore Layout.layout
        |> Combine.continueWithCore (Tokens.typeName |> Core.map List.singleton |> Node.parserCore)
        |> Combine.ignore Layout.optimisticLayout


importInnerParseExposingDefinition : Location -> Node ModuleName -> Maybe (Node ModuleName) -> Parser State (Node Import)
importInnerParseExposingDefinition start mod asDef =
    Combine.oneOf
        [ exposeDefinitionNode
            |> Combine.map
                (\exposing_ ->
                    setupNode start { moduleName = mod, moduleAlias = asDef, exposingList = Just exposing_ }
                )
        , Combine.succeedLazy
            (\() ->
                setupNode start { moduleName = mod, moduleAlias = asDef, exposingList = Nothing }
            )
        ]


exposeDefinitionNode : Parser State (Node Exposing)
exposeDefinitionNode =
    Node.parser exposeDefinition


importInnerParseAsDefinition : Location -> Node ModuleName -> Parser State (Node Import)
importInnerParseAsDefinition start mod =
    Combine.oneOf
        [ importInnerAsDefinitionOptimisticLayout
            |> Combine.andThen (\alias_ -> importInnerParseExposingDefinition start mod (Just alias_))
        , importInnerParseExposingDefinition start mod Nothing
        ]


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
