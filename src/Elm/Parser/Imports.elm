module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


importDefinition : Parser (WithComments (Node Import))
importDefinition =
    Core.map
        (\startRow ->
            \commentsAfterImport ->
                \((Node modRange _) as mod) ->
                    \commentsAfterModuleName ->
                        \moduleAlias ->
                            \exposingList ->
                                \commentsAfterEverything ->
                                    let
                                        endRange : Range
                                        endRange =
                                            case moduleAlias.syntax of
                                                Just (Node range _) ->
                                                    range

                                                Nothing ->
                                                    case exposingList.syntax of
                                                        Just (Node range _) ->
                                                            range

                                                        Nothing ->
                                                            modRange
                                    in
                                    { comments =
                                        Rope.flatFromList
                                            [ commentsAfterImport
                                            , commentsAfterModuleName
                                            , moduleAlias.comments
                                            , exposingList.comments
                                            , commentsAfterEverything
                                            ]
                                    , syntax =
                                        Node
                                            { start = { row = startRow, column = 1 }, end = endRange.end }
                                            { moduleName = mod
                                            , moduleAlias = moduleAlias.syntax
                                            , exposingList = exposingList.syntax
                                            }
                                    }
        )
        Core.getRow
        |. Tokens.importToken
        |= Layout.layout
        |= moduleName
        |= Layout.optimisticLayout
        |= ParserWithComments.maybe
            ((Tokens.asToken
                |> Parser.Extra.continueWith
                    (Core.map
                        (\commentsBefore ->
                            \moduleAlias ->
                                \commentsAfter ->
                                    { comments = Rope.flatFromList [ commentsBefore, commentsAfter ]
                                    , syntax = moduleAlias
                                    }
                        )
                        Layout.layout
                    )
             )
                |= (Tokens.typeName |> Node.parserCoreValueMap List.singleton)
                |= Layout.optimisticLayout
            )
        |= ParserWithComments.maybe (Node.parser exposeDefinition)
        |= Layout.optimisticLayout
