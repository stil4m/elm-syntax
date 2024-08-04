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
    CustomParser.map
        (\() ->
            \startRow ->
                \commentsAfterImport ->
                    \((Node modRange _) as mod) ->
                        \commentsAfterModuleName ->
                            \maybeModuleAlias ->
                                \maybeExposingList ->
                                    \commentsAfterEverything ->
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
                                        , syntax =
                                            Node
                                                { start = { row = startRow, column = 1 }, end = endRange.end }
                                                { moduleName = mod
                                                , moduleAlias = maybeModuleAlias |> Maybe.map .syntax
                                                , exposingList = maybeExposingList |> Maybe.map .syntax
                                                }
                                        }
        )
        Tokens.importToken
        |> CustomParser.keep CustomParser.getRow
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep moduleName
        |> CustomParser.keep Layout.optimisticLayout
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\() ->
                        \commentsBefore ->
                            \moduleAliasStart ->
                                \moduleAlias ->
                                    \commentsAfter ->
                                        Just
                                            { comments = commentsBefore |> Rope.prependTo commentsAfter
                                            , syntax =
                                                Node
                                                    (Node.singleLineStringRangeFrom
                                                        moduleAliasStart
                                                        moduleAlias
                                                    )
                                                    [ moduleAlias ]
                                            }
                    )
                    Tokens.asToken
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep CustomParser.getPosition
                    |> CustomParser.keep Tokens.typeName
                    |> CustomParser.keep Layout.optimisticLayout
                , CustomParser.succeed Nothing
                ]
            )
        |> CustomParser.keep
            (CustomParser.oneOf
                [ Node.parserMapWithComments Just exposeDefinition
                , CustomParser.succeed Nothing
                ]
            )
        |> CustomParser.keep Layout.optimisticLayout
