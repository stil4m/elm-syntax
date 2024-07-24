module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (moduleName)
import Elm.Parser.Expose exposing (exposeDefinition)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser as Core exposing ((|.))


importDefinition : Parser State (Node Import)
importDefinition =
    Core.map
        (\( startRow, startColumn ) ->
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
                            { start = { row = startRow, column = startColumn }, end = endRange.end }
                            { moduleName = mod, moduleAlias = moduleAlias, exposingList = exposingList }
        )
        Core.getPosition
        |. Tokens.importToken
        |> Combine.fromCoreIgnore Layout.layout
        |> Combine.keepFromCore moduleName
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.keep
            (Combine.maybe
                (Tokens.asToken
                    |> Combine.fromCoreIgnore Layout.layout
                    |> Combine.continueWithCore (Tokens.typeName |> Node.parserCoreValueMap List.singleton)
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
            )
        |> Combine.keep (Combine.maybe (Node.parser exposeDefinition))
        |> Combine.ignore Layout.optimisticLayout
