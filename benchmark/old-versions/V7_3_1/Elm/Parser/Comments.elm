module V7_3_1.Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Parser as Core exposing (Nestable(..))
import V7_3_1.Combine as Combine exposing (Parser, modifyState, string, succeed)
import V7_3_1.Elm.Parser.Node as Node
import V7_3_1.Elm.Parser.State exposing (State, addComment)
import V7_3_1.Elm.Parser.Whitespace exposing (untilNewlineToken)
import V7_3_1.Elm.Syntax.Node exposing (Node)


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair) |> Combine.continueWith (succeed ()))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            |> Combine.andMap (string "--")
            |> Combine.andMap untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    Core.getChompedString (Core.multiComment "{-" "-}" Nestable)
        |> Combine.fromCore


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner
