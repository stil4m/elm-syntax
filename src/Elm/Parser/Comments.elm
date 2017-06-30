module Elm.Parser.Comments exposing (singleLineComment, multilineComment)

import Combine exposing (Parser, string, lazy, sequence, (<$>), manyTill, succeed, (<*>), (*>), (>>=), lookAhead, count, modifyState)
import Combine.Char exposing (anyChar)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Syntax.Range exposing (..)


addCommentToState : Parser State ( String, Range ) -> Parser State ()
addCommentToState p =
    p >>= \pair -> modifyState (addComment pair) *> succeed ()


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    withRange
        ((,) <$> commentParser)
        |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            <*> string "--"
            <*> untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    lazy
        (\() ->
            String.concat
                <$> sequence
                        [ string "{-"
                        , String.concat
                            <$> manyTill
                                    (lookAhead (count 2 anyChar)
                                        >>= \x ->
                                                if x == [ '{', '-' ] then
                                                    multilineCommentInner
                                                else
                                                    String.fromChar <$> anyChar
                                    )
                                    (string "-}")
                        , succeed "-}"
                        ]
        )


multilineComment : Parser State ()
multilineComment =
    lazy (\() -> parseComment multilineCommentInner)
