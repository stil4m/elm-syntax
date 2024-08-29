module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope
import ParserWithComments exposing (Comments)


importDefinition : Parser (WithComments (Node Import))
importDefinition =
    ParserFast.map6WithStartLocation
        (\start commentsAfterImport ((Node modRange _) as mod) commentsAfterModuleName maybeModuleAlias maybeExposingList commentsAfterEverything ->
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
                let
                    commentsBeforeAlias : Comments
                    commentsBeforeAlias =
                        commentsAfterImport
                            |> Rope.prependTo commentsAfterModuleName

                    commentsBeforeExposingList : Comments
                    commentsBeforeExposingList =
                        case maybeModuleAlias of
                            Nothing ->
                                commentsBeforeAlias

                            Just moduleAliasValue ->
                                commentsBeforeAlias |> Rope.prependTo moduleAliasValue.comments
                in
                (case maybeExposingList of
                    Nothing ->
                        commentsBeforeExposingList

                    Just exposingListValue ->
                        commentsBeforeExposingList |> Rope.prependTo exposingListValue.comments
                )
                    |> Rope.prependTo commentsAfterEverything
            , syntax =
                Node { start = start, end = endRange.end }
                    { moduleName = mod
                    , moduleAlias = maybeModuleAlias |> Maybe.map .syntax
                    , exposingList = maybeExposingList |> Maybe.map .syntax
                    }
            }
        )
        (ParserFast.keywordFollowedBy "import" Layout.maybeLayout)
        moduleName
        Layout.optimisticLayout
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> Rope.prependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
            )
            (ParserFast.keywordFollowedBy "as" Layout.maybeLayout)
            (Tokens.typeNameMapWithRange
                (\range moduleAlias ->
                    Node range [ moduleAlias ]
                )
            )
            Layout.optimisticLayout
            Nothing
        )
        (ParserFast.mapOrSucceed
            Just
            exposeDefinition
            Nothing
        )
        Layout.optimisticLayout
