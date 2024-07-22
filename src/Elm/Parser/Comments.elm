module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

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


parseComment : Core.Parser String -> Parser State ()
parseComment commentParser =
    Node.parserCore commentParser
        |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (Core.symbol "--"
            |. untilNewline
            |> Core.getChompedString
        )


untilNewline : Core.Parser ()
untilNewline =
    Core.chompWhile (\c -> c /= '\u{000D}' && c /= '\n')


multilineCommentInner : Core.Parser String
multilineCommentInner =
    Core.oneOf
        [ Core.symbol "{-|"
            |> Core.backtrackable
            |> Core.map (\() -> Core.problem "unexpected multiline comment")
        , Core.multiComment "{-" "-}" Nestable
            |> Core.mapChompedString (\comment () -> Core.succeed comment)
        ]
        |> Core.andThen identity


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner


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
