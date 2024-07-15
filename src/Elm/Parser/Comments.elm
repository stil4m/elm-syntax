module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Combine exposing (Parser, modifyState)
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=), Nestable(..))


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (Core.symbol "--"
            |. untilNewlineToken
            |> Core.getChompedString
            |> Combine.fromCore
        )


multilineCommentInner : Parser State String
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
        |> Combine.fromCore


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner


moduleDocumentation : Parser State ()
moduleDocumentation =
    declarationDocumentation |> addCommentToState


declarationDocumentation : Parser State (Node Documentation)
declarationDocumentation =
    Core.getChompedString (Core.multiComment "{-|" "-}" Nestable)
        |> Combine.fromCore
        |> Node.parser
