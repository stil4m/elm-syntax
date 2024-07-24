module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineCommentString, singleLineCommentCore)

import Combine exposing (Parser)
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State, addComment)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), Nestable(..))


addCommentToState : Core.Parser (Node String) -> Parser State ()
addCommentToState p =
    p
        |> Combine.andThenFromCore
            (\pair ->
                Combine.modifyState (\state -> addComment pair state)
            )


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


moduleDocumentation : Parser State ()
moduleDocumentation =
    addCommentToState declarationDocumentation


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
