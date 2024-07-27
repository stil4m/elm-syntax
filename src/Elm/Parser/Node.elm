module Elm.Parser.Node exposing (parser, parserCore, parserCoreMap, parserMap, parserMapWithComments, singleLineStringFrom, singleLineStringRangeFrom)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|=), Parser)
import ParserWithComments exposing (WithComments)


singleLineStringRangeFrom : Location -> String -> Range
singleLineStringRangeFrom start string =
    { start = start
    , end = { row = start.row, column = start.column + String.length string }
    }


singleLineStringFrom : Location -> String -> Node String
singleLineStringFrom start string =
    Node
        { start = start
        , end = { row = start.row, column = start.column + String.length string }
        }
        string


parserMapWithComments : (WithComments (Node a) -> b) -> Parser (WithComments a) -> Parser b
parserMapWithComments valueNodeChange p =
    Parser.map
        (\( startRow, startColumn ) v ( endRow, endColumn ) ->
            { comments = v.comments
            , syntax =
                Node
                    { start = { row = startRow, column = startColumn }
                    , end = { row = endRow, column = endColumn }
                    }
                    v.syntax
            }
                |> valueNodeChange
        )
        Parser.getPosition
        |= p
        |= Parser.getPosition


parserMap : (Node a -> b) -> Parser (WithComments a) -> Parser (WithComments b)
parserMap valueNodeChange p =
    Parser.map
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
        Parser.getPosition
        |= p
        |= Parser.getPosition


parser : Parser (WithComments a) -> Parser (WithComments (Node a))
parser p =
    Parser.map
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
        Parser.getPosition
        |= p
        |= Parser.getPosition


{-| Internally saves 1 Parser.map compared to parserCore |> Parser.map
-}
parserCoreMap : (Node a -> b) -> Parser.Parser a -> Parser.Parser b
parserCoreMap valueNodeChange p =
    Parser.map
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
        Parser.getPosition
        |= p
        |= Parser.getPosition


parserCore : Parser.Parser a -> Parser.Parser (Node a)
parserCore p =
    Parser.map
        (\( startRow, startColumn ) ->
            \v ->
                \( endRow, endColumn ) ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
                        v
        )
        Parser.getPosition
        |= p
        |= Parser.getPosition
