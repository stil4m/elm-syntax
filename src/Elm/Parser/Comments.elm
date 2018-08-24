module Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Combine exposing ((*>), (<$>), (<*>), (>>=), Parser, count, lazy, lookAhead, manyTill, modifyState, sequence, string, succeed)
import Combine.Char exposing (anyChar)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Ranged exposing (Ranged)


addCommentToState : Parser State (Ranged String) -> Parser State ()
addCommentToState p =
    p >>= (\pair -> modifyState (addComment pair) *> succeed ())


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    withRange
        ((\b a -> (\a b -> ( a, b )) a b) <$> commentParser)
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
                                        >>= (\x ->
                                                if x == [ '{', '-' ] then
                                                    multilineCommentInner

                                                else
                                                    String.fromChar <$> anyChar
                                            )
                                    )
                                    (string "-}")
                        , succeed "-}"
                        ]
        )


multilineComment : Parser State ()
multilineComment =
    lazy (\() -> parseComment multilineCommentInner)
