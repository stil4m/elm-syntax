module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
    )

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Parser exposing ((|.), (|=), Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


{-| Nothing if there can't be more whitespace/comments ahead,
otherwise Just with the next comment node
-}
whitespaceAndCommentsOrEmpty : Parser.Parser Comments
whitespaceAndCommentsOrEmpty =
    Parser.oneOf
        [ whitespace
            -- whitespace can't be followed by more whitespace
            |> Parser.andThen (\_ -> fromCommentElseEmpty)
        , -- below will never run with elm-format-ed code
          fromCommentElseEmpty
        ]


whitespace : Parser String
whitespace =
    Parser.variable
        { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        , reserved = Set.empty
        , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        }


fromCommentElseEmpty : Parser Comments
fromCommentElseEmpty =
    -- since comments are comparatively rare
    -- but expensive to check for, we allow shortcutting to dead end
    Parser.map
        (\source offset ->
            case source |> String.slice offset (offset + 2) of
                "--" ->
                    -- this will always succeed from here, so no need to fall back to Rope.empty
                    fromSingleLineCommentNode

                "{-" ->
                    fromMultilineCommentNodeOrEmptyOnProblem

                _ ->
                    succeedRopeEmpty
        )
        Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen identity


succeedRopeEmpty : Parser (Rope.Rope a)
succeedRopeEmpty =
    Parser.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    Parser.oneOf [ fromMultilineCommentNode, Parser.succeed Rope.empty ]


fromMultilineCommentNode : Parser Comments
fromMultilineCommentNode =
    Node.parserCoreMap
        (\comment ->
            \commentsAfter ->
                Rope.flatFromList [ Rope.one comment, commentsAfter ]
        )
        Comments.multilineCommentString
        |= whitespaceAndCommentsOrEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    Parser.map
        (\comment ->
            \commentsAfter ->
                Rope.flatFromList [ Rope.one comment, commentsAfter ]
        )
        Comments.singleLineCommentCore
        |= whitespaceAndCommentsOrEmpty


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty
        |. verifyLayoutIndent


verifyLayoutIndent : Parser ()
verifyLayoutIndent =
    verifyIndent (\stateIndent current -> stateIndent < current)
        (\stateIndent current -> "Expected indent larger than " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


positivelyIndented : Parser.Parser ()
positivelyIndented =
    Parser.map
        (\column ->
            \indent ->
                if indent < column then
                    succeedUnit

                else
                    problemPositivelyIndented
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


problemPositivelyIndented : Parser a
problemPositivelyIndented =
    Parser.problem "must be positively indented"


layout : Parser Comments
layout =
    Parser.oneOf
        [ (whitespace
            |> Parser.andThen (\_ -> fromCommentElseEmpty)
          )
            |. verifyLayoutIndent
        , -- below will never run with elm-format-ed code
          Parser.map
            (\source offset ->
                case source |> String.slice offset (offset + 2) of
                    "--" ->
                        -- this will always succeed from here, so no need to fall back to Rope.empty
                        fromSingleLineCommentNodeVerifyLayoutIndent

                    "{-" ->
                        fromMultilineCommentNodeOrEmptyOnProblemVerifyLayoutIndent

                    _ ->
                        problemMissingWhitespaceOrComments
            )
            Parser.getSource
            |= Parser.getOffset
            |> Parser.andThen identity
        ]


fromSingleLineCommentNodeVerifyLayoutIndent : Parser Comments
fromSingleLineCommentNodeVerifyLayoutIndent =
    fromSingleLineCommentNode |. verifyLayoutIndent


fromMultilineCommentNodeOrEmptyOnProblemVerifyLayoutIndent : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblemVerifyLayoutIndent =
    fromMultilineCommentNodeOrEmptyOnProblem |. verifyLayoutIndent


problemMissingWhitespaceOrComments : Parser a
problemMissingWhitespaceOrComments =
    Parser.problem "missing whitespace/comments"


optimisticLayout : Parser Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |. verifyIndent (\stateIndent current -> stateIndent == current)
            (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


onTopIndentation : res -> Parser res
onTopIndentation res =
    Parser.map
        (\column ->
            \indent ->
                if indent == column then
                    Parser.succeed res

                else
                    problemTopIndentation
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


problemTopIndentation : Parser.Parser a
problemTopIndentation =
    Parser.problem "must be on top indentation"


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser ()
verifyIndent verify failMessage =
    Parser.map
        (\column ->
            \indent ->
                if verify indent column then
                    succeedUnit

                else
                    Parser.problem (failMessage indent column)
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


succeedUnit : Parser ()
succeedUnit =
    Parser.succeed ()


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    Parser.map
        (\before ->
            \v ->
                \after ->
                    { comments = Rope.flatFromList [ before, v.comments, after ]
                    , syntax = v.syntax
                    }
        )
        maybeLayout
        |= x
        |= maybeLayout
