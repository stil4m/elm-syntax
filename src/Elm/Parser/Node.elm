module Elm.Parser.Node exposing (parser, parserCore, parserMapWithComments)

import CustomParser exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import ParserWithComments exposing (WithComments)


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
