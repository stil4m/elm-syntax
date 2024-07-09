module Elm.Parser.Comments exposing (declarationDocumentation, moduleDocumentation, multilineComment, singleLineComment)

import Combine exposing (Parser, modifyState, string, succeed)
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing (Nestable(..))


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            |> Combine.keep (string "--")
            |> Combine.keep untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    Core.getChompedString (Core.multiComment "{-" "-}" Nestable)
        |> Combine.fromCore
        |> Combine.backtrackable
        |> Combine.andThen
            (\comment ->
                if String.startsWith "{-|" comment then
                    Combine.fail "unexpected documentation comment"

                else
                    Combine.fromCore (Core.commit ())
                        |> Combine.continueWith (Combine.succeed comment)
            )


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
