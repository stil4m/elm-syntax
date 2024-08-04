module Elm.Parser.Node exposing (parser, parserCore, parserMapWithComments)

import CustomParser exposing (Parser)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import ParserWithComments exposing (WithComments)


parserMapWithComments : (WithComments (Node a) -> b) -> Parser (WithComments a) -> Parser b
parserMapWithComments valueNodeChange p =
    CustomParser.map3
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
        CustomParser.getPosition
        p
        CustomParser.getPosition


parserMap : (Node a -> b) -> Parser (WithComments a) -> Parser (WithComments b)
parserMap valueNodeChange p =
    CustomParser.map3
        (\start v end ->
            { comments = v.comments
            , syntax =
                Node { start = start, end = end }
                    v.syntax
                    |> valueNodeChange
            }
        )
        CustomParser.getPosition
        p
        CustomParser.getPosition


parser : Parser (WithComments a) -> Parser (WithComments (Node a))
parser p =
    CustomParser.map3
        (\start v end ->
            { comments = v.comments
            , syntax =
                Node { start = start, end = end } v.syntax
            }
        )
        CustomParser.getPosition
        p
        CustomParser.getPosition


{-| Internally saves 1 CustomParser.map compared to parserCore |> CustomParser.map
-}
parserCoreMap : (Node a -> b) -> CustomParser.Parser a -> CustomParser.Parser b
parserCoreMap valueNodeChange p =
    CustomParser.map3
        (\start v end ->
            Node { start = start, end = end } v
                |> valueNodeChange
        )
        CustomParser.getPosition
        p
        CustomParser.getPosition


parserCore : CustomParser.Parser a -> CustomParser.Parser (Node a)
parserCore p =
    CustomParser.map3
        (\start v end ->
            Node { start = start, end = end } v
        )
        CustomParser.getPosition
        p
        CustomParser.getPosition
