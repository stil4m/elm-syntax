module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser as Core exposing ((|.))
import ParserWithComments exposing (ParserWithComments)


importDefinition : ParserWithComments (Node Import)
importDefinition =
    Core.map
        (\startRow ->
            \((Node modRange _) as mod) ->
                \moduleAlias ->
                    \exposingList ->
                        let
                            endRange : Range
                            endRange =
                                case moduleAlias of
                                    Just (Node range _) ->
                                        range

                                    Nothing ->
                                        case exposingList of
                                            Just (Node range _) ->
                                                range

                                            Nothing ->
                                                modRange
                        in
                        Node
                            { start = { row = startRow, column = 1 }, end = endRange.end }
                            { moduleName = mod, moduleAlias = moduleAlias, exposingList = exposingList }
        )
        Core.getRow
        |. Tokens.importToken
        |> ParserWithComments.fromCoreIgnore Layout.layout
        |> ParserWithComments.keepFromCore moduleName
        |> ParserWithComments.ignore Layout.optimisticLayout
        |> ParserWithComments.keep
            (ParserWithComments.maybe
                (Tokens.asToken
                    |> ParserWithComments.fromCoreIgnore Layout.layout
                    |> ParserWithComments.continueWithCore (Tokens.typeName |> Node.parserCoreValueMap List.singleton)
                    |> ParserWithComments.ignore Layout.optimisticLayout
                )
            )
        |> ParserWithComments.keep (ParserWithComments.maybe (Node.parser exposeDefinition))
        |> ParserWithComments.ignore Layout.optimisticLayout
