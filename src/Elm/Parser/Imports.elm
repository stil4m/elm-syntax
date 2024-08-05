module Elm.Parser.Imports exposing (importDefinition)

import CustomParser exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import ParserWithComments exposing (WithComments)
import Rope


importDefinition : Parser (WithComments (Node Import))
importDefinition =
    CustomParser.mapWithStartPosition
        (\start importResult ->
            { comments = importResult.comments
            , syntax =
                Node { start = start, end = importResult.end }
                    importResult.import_
            }
        )
        (CustomParser.map6
            (\commentsAfterImport ((Node modRange _) as mod) commentsAfterModuleName maybeModuleAlias maybeExposingList commentsAfterEverything ->
                let
                    endRange : Range
                    endRange =
                        case maybeModuleAlias of
                            Just moduleAliasValue ->
                                let
                                    (Node range _) =
                                        moduleAliasValue.syntax
                                in
                                range

                            Nothing ->
                                case maybeExposingList of
                                    Just exposingListValue ->
                                        let
                                            (Node range _) =
                                                exposingListValue.syntax
                                        in
                                        range

                                    Nothing ->
                                        modRange
                in
                { comments =
                    commentsAfterImport
                        |> Rope.prependTo commentsAfterModuleName
                        |> Rope.prependTo
                            (case maybeModuleAlias of
                                Nothing ->
                                    Rope.empty

                                Just moduleAliasValue ->
                                    moduleAliasValue.comments
                            )
                        |> Rope.prependTo
                            (case maybeExposingList of
                                Nothing ->
                                    Rope.empty

                                Just exposingListValue ->
                                    exposingListValue.comments
                            )
                        |> Rope.prependTo commentsAfterEverything
                , end = endRange.end
                , import_ =
                    { moduleName = mod
                    , moduleAlias = maybeModuleAlias |> Maybe.map .syntax
                    , exposingList = maybeExposingList |> Maybe.map .syntax
                    }
                }
            )
            (CustomParser.keywordFollowedBy "import" Layout.maybeLayout)
            moduleName
            Layout.optimisticLayout
            (CustomParser.orSucceed
                (CustomParser.map3
                    (\commentsBefore moduleAliasNode commentsAfter ->
                        Just
                            { comments = commentsBefore |> Rope.prependTo commentsAfter
                            , syntax = moduleAliasNode
                            }
                    )
                    (CustomParser.keywordFollowedBy "as" Layout.maybeLayout)
                    (CustomParser.mapWithStartAndEndPosition
                        (\start moduleAlias end ->
                            Node { start = start, end = end }
                                [ moduleAlias ]
                        )
                        Tokens.typeName
                    )
                    Layout.optimisticLayout
                )
                Nothing
            )
            (CustomParser.orSucceed
                (Node.parserMapWithComments Just exposeDefinition)
                Nothing
            )
            Layout.optimisticLayout
        )
