module Elm.Parser.Node exposing (parser, parserCore)

import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)


parser : Parser (WithComments a) -> Parser (WithComments (Node a))
parser p =
    ParserFast.mapWithStartAndEndPosition
        (\start v end ->
            { comments = v.comments
            , syntax =
                Node { start = start, end = end } v.syntax
            }
        )
        p


parserCore : ParserFast.Parser a -> ParserFast.Parser (Node a)
parserCore p =
    ParserFast.mapWithStartAndEndPosition
        (\start v end ->
            Node { start = start, end = end } v
        )
        p
