module Elm.Parser.Node exposing (parser, parserCore)

import Combine exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|=))


parser : Parser state a -> Parser state (Node a)
parser p =
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
        |> Combine.fromCoreKeep p
        |> Combine.keepFromCore Core.getPosition


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
