module Elm.Parser.Node exposing (parser, parserCore, parserCoreMap, parserCoreValueMap, parserMap)

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))


parserMap : (Node a -> b) -> Parser state a -> Parser state b
parserMap valueNodeChange p =
    Combine.map3CoreCombineCore
        (\( startRow, startColumn ) v ( endRow, endColumn ) ->
            Node
                { start = { row = startRow, column = startColumn }
                , end = { row = endRow, column = endColumn }
                }
                v
                |> valueNodeChange
        )
        Core.getPosition
        p
        Core.getPosition


parser : Parser state a -> Parser state (Node a)
parser p =
    Combine.map3CoreCombineCore
        (\( startRow, startColumn ) v ( endRow, endColumn ) ->
            Node
                { start = { row = startRow, column = startColumn }
                , end = { row = endRow, column = endColumn }
                }
                v
        )
        Core.getPosition
        p
        Core.getPosition


{-| Internally saves 1 Core.map compared to parserCore |> Core.map
-}
parserCoreMap : (Node a -> b) -> Core.Parser a -> Core.Parser b
parserCoreMap valueNodeChange p =
    Core.map
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        v
                        |> valueNodeChange
        )
        Core.getPosition
        |= p
        |= Core.getPosition


{-| Internally saves 1 Core.map compared to parserCore |> Core.map
-}
parserCoreValueMap : (a -> b) -> Core.Parser a -> Core.Parser (Node b)
parserCoreValueMap valueChange p =
    Core.map
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        (v |> valueChange)
        )
        Core.getPosition
        |= p
        |= Core.getPosition


parserCore : Core.Parser a -> Core.Parser (Node a)
parserCore p =
    Core.map
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        v
        )
        Core.getPosition
        |= p
        |= Core.getPosition
