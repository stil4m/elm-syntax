module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Elm.Parser.Node as Node
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), Nestable(..))
import ParserWithComments exposing (ParserWithComments)
import Rope


singleLineCommentCore : Core.Parser ()
singleLineCommentCore =
    Core.symbol "--"
        |. Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')


multilineCommentString : Core.Parser String
multilineCommentString =
    Core.oneOf
        [ Core.symbol "{-|"
            |> Core.backtrackable
            |> Core.map (\() -> problemUnexpected)
        , Core.multiComment "{-" "-}" Nestable
            |> Core.mapChompedString (\comment () -> Core.succeed comment)
        ]
        |> Core.andThen identity


problemUnexpected : Core.Parser a
problemUnexpected =
    Core.problem "unexpected documentation comment"


moduleDocumentation : ParserWithComments ()
moduleDocumentation =
    declarationDocumentation
        |> Core.map
            (\comment ->
                { comments = Rope.one comment
                , syntax = ()
                }
            )


declarationDocumentation : Core.Parser (Node Documentation)
declarationDocumentation =
    Core.oneOf
        [ Core.oneOf
            [ Core.symbol "{-|"
                |> Core.backtrackable
                |> Core.map (\() -> coreTemporaryProblem {- "falls back" to multiComment from the start -})
            , Core.succeed (Core.succeed (Core.problem "not a documentation comment"))
            ]
            |> Core.andThen identity
        , Core.multiComment "{-" "-}" Nestable
            |> Core.mapChompedString (\comment () -> Core.succeed comment)
        ]
        |> Core.andThen identity
        |> Node.parserCore


coreTemporaryProblem : Core.Parser a
coreTemporaryProblem =
    Core.problem ""
