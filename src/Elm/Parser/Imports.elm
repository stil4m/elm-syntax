module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


importDefinition : Parser (WithComments (Node Import))
importDefinition =
    Parser.map
        (\startRow ->
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
                                        Rope.flatFromList
                                            [ commentsAfterImport
                                            , commentsAfterModuleName
                                            , case maybeModuleAlias of
                                                Nothing ->
                                                    Rope.empty

                                                Just moduleAliasValue ->
                                                    moduleAliasValue.comments
                                            , case maybeExposingList of
                                                Nothing ->
                                                    Rope.empty

                                                Just exposingListValue ->
                                                    exposingListValue.comments
                                            , commentsAfterEverything
                                            ]
                                    , syntax =
                                        Node
                                            { start = { row = startRow, column = 1 }, end = endRange.end }
                                            { moduleName = mod
                                            , moduleAlias = maybeModuleAlias |> Maybe.map .syntax
                                            , exposingList = maybeExposingList |> Maybe.map .syntax
                                            }
                                    }
        )
        Parser.getRow
        |. Tokens.importToken
        |= Layout.layout
        |= moduleName
        |= Layout.optimisticLayout
        |= Parser.oneOf
            [ (Tokens.asToken
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsBefore ->
                            \( moduleAliasStartRow, moduleAliasStartColumn ) ->
                                \moduleAlias ->
                                    \commentsAfter ->
                                        Just
                                            { comments = Rope.flatFromList [ commentsBefore, commentsAfter ]
                                            , syntax =
                                                Node
                                                    (Node.singleLineStringRangeFrom
                                                        { row = moduleAliasStartRow, column = moduleAliasStartColumn }
                                                        moduleAlias
                                                    )
                                                    [ moduleAlias ]
                                            }
                        )
                        Layout.layout
                    )
              )
                |= Parser.getPosition
                |= Tokens.typeName
                |= Layout.optimisticLayout
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Node.parserMapWithComments Just exposeDefinition
            , Parser.succeed Nothing
            ]
        |= Layout.optimisticLayout
