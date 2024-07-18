module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Combine exposing (Parser)
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=), Nestable(..))


addCommentToState : Core.Parser (Node String) -> Parser State ()
addCommentToState p =
    p
        |> Combine.fromCore
        |> Combine.andThen (\pair -> Combine.modifyState (\state -> addComment pair state))


parseComment : Core.Parser String -> Parser State ()
parseComment commentParser =
    Node.parserCore commentParser
        |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (Core.symbol "--"
            |. untilNewlineToken
            |> Core.getChompedString
        )


multilineCommentInner : Core.Parser String
multilineCommentInner =
    Core.succeed (\offset -> \source -> String.slice offset (offset + 3) source)
        |= Core.getOffset
        |= Core.getSource
        |> Core.andThen
            (\opening ->
                if String.startsWith "{-" opening && opening /= "{-|" then
                    Core.multiComment "{-" "-}" Nestable

                else
                    Core.problem "unexpected documentation comment"
            )
        |> Core.getChompedString


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner


moduleDocumentation : Parser State ()
moduleDocumentation =
    addCommentToState declarationDocumentation


declarationDocumentation : Core.Parser (Node Documentation)
declarationDocumentation =
    Core.succeed (\offset -> \source -> String.slice offset (offset + 3) source)
        |= Core.getOffset
        |= Core.getSource
        |> Core.andThen
            (\opening ->
                if opening == "{-|" then
                    Core.multiComment "{-" "-}" Nestable

                else
                    Core.problem "not a documentation comment"
            )
        |> Core.getChompedString
        |> Node.parserCore
