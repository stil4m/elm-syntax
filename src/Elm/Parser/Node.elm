module Elm.Parser.Node exposing (parser, parserCore, parserMapWithComments, singleLineStringFrom, singleLineStringRangeFrom)

import CustomParser exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
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
    CustomParser.mapWithStartAndEndPosition
        (\start v end ->
            { comments = v.comments
            , syntax =
                Node
                    { start = start
                    , end = end
                    }
                    v.syntax
            }
                |> valueNodeChange
        )
        p


parser : Parser (WithComments a) -> Parser (WithComments (Node a))
parser p =
    CustomParser.mapWithStartAndEndPosition
        (\start v end ->
            { comments = v.comments
            , syntax =
                Node { start = start, end = end } v.syntax
            }
        )
        p


parserCore : CustomParser.Parser a -> CustomParser.Parser (Node a)
parserCore p =
    CustomParser.mapWithStartAndEndPosition
        (\start v end ->
            Node { start = start, end = end } v
        )
        p
