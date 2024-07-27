module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|.), (|=), Nestable(..), Parser)
import Rope exposing (RopeLikelyFilled)


singleLineCommentCore : Parser.Parser (Node String)
singleLineCommentCore =
    Parser.map
        (\( startRow, startColumn ) ->
            \content ->
                \endColumn ->
                    Node
                        { start = { row = startRow, column = startColumn }
                        , end = { row = startRow, column = endColumn }
                        }
                        content
        )
        Parser.getPosition
        |= (Parser.symbol "--"
                |. Parser.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')
                |> Parser.getChompedString
           )
        |= Parser.getCol


multilineCommentString : Parser.Parser String
multilineCommentString =
    Parser.oneOf
        [ Parser.symbol "{-|"
            |> Parser.backtrackable
            |> Parser.map (\() -> problemUnexpected)
        , Parser.multiComment "{-" "-}" Nestable
            |> Parser.mapChompedString (\comment () -> Parser.succeed comment)
        ]
        |> Parser.andThen identity


problemUnexpected : Parser.Parser a
problemUnexpected =
    Parser.problem "unexpected documentation comment"


moduleDocumentation : Parser (RopeLikelyFilled (Node String))
moduleDocumentation =
    declarationDocumentation
        |> Parser.map (\comment -> Rope.one comment)


declarationDocumentation : Parser.Parser (Node Documentation)
declarationDocumentation =
    Parser.oneOf
        [ Parser.oneOf
            [ Parser.symbol "{-|"
                |> Parser.backtrackable
                |> Parser.map (\() -> coreTemporaryProblem {- "falls back" to multiComment from the start -})
            , Parser.succeed (Parser.succeed (Parser.problem "not a documentation comment"))
            ]
            |> Parser.andThen identity
        , Parser.multiComment "{-" "-}" Nestable
            |> Parser.mapChompedString (\comment () -> Parser.succeed comment)
        ]
        |> Parser.andThen identity
        |> Node.parserCore


coreTemporaryProblem : Parser.Parser a
coreTemporaryProblem =
    Parser.problem ""
