module Elm.Parser.Node exposing (parser, parserCore, parserCoreMap, parserCoreValueMap, parserMap)

import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=), Parser)
import ParserWithComments exposing (WithComments)


parserMap : (Node a -> b) -> Parser (WithComments a) -> Parser (WithComments b)
parserMap valueNodeChange p =
    Core.map
        (\( startRow, startColumn ) v ( endRow, endColumn ) ->
            { comments = v.comments
            , syntax =
                Node
                    { start = { row = startRow, column = startColumn }
                    , end = { row = endRow, column = endColumn }
                    }
                    v.syntax
                    |> valueNodeChange
            }
        )
        Core.getPosition
        |= p
        |= Core.getPosition


parser : Parser (WithComments a) -> Parser (WithComments (Node a))
parser p =
    Core.map
        (\( startRow, startColumn ) v ( endRow, endColumn ) ->
            { comments = v.comments
            , syntax =
                Node
                    { start = { row = startRow, column = startColumn }
                    , end = { row = endRow, column = endColumn }
                    }
                    v.syntax
            }
        )
        Core.getPosition
        |= p
        |= Core.getPosition


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
