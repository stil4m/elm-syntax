module Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Combine exposing (Parser, count, lazy, lookAhead, manyTill, modifyState, sequence, string, succeed)
import Combine.Char exposing (anyChar)
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Ranged exposing (Ranged)


addCommentToState : Parser State (Ranged String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair) |> Combine.continueWith (succeed ()))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    withRange
        (Combine.map (\a b -> ( b, a )) commentParser)
        |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            |> Combine.andMap (string "--")
            |> Combine.andMap untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    lazy
        (\() ->
            Combine.map String.concat
                (sequence
                    [ string "{-"
                    , Combine.map String.concat
                        (manyTill
                            (lookAhead (count 2 anyChar)
                                |> Combine.andThen
                                    (\x ->
                                        if x == [ '{', '-' ] then
                                            multilineCommentInner

                                        else
                                            Combine.map String.fromChar anyChar
                                    )
                            )
                            (string "-}")
                        )
                    , succeed "-}"
                    ]
                )
        )


multilineComment : Parser State ()
multilineComment =
    lazy (\() -> parseComment multilineCommentInner)
