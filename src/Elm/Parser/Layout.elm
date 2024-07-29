module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , moduleLevelIndentation
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


whitespaceAndCommentsOrEmpty : Parser.Parser Comments
whitespaceAndCommentsOrEmpty =
    Parser.oneOf
        [ whitespace
            -- whitespace can't be followed by more whitespace
            |> Parser.andThen (\_ -> fromCommentElseEmpty)
        , fromCommentElseEmpty
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
        (\source ->
            \offset ->
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
                Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.multilineCommentString
        |= whitespaceAndCommentsOrEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    Parser.map
        (\comment ->
            \commentsAfter ->
                Rope.one comment |> Rope.filledPrependTo commentsAfter
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
            (\source ->
                \offset ->
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


moduleLevelIndentation : res -> Parser res
moduleLevelIndentation res =
    Parser.andThen
        (\column ->
            if column == 1 then
                Parser.succeed res

            else
                problemModuleLevelIndentation
        )
        Parser.getCol


problemModuleLevelIndentation : Parser.Parser a
problemModuleLevelIndentation =
    Parser.problem "must be on module-level indentation"


onTopIndentation : res -> Parser res
onTopIndentation res =
    Parser.map
        (\column ->
            \indent ->
                if column == indent then
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
                    { comments =
                        before
                            |> Rope.prependTo v.comments
                            |> Rope.prependTo after
                    , syntax = v.syntax
                    }
        )
        maybeLayout
        |= x
        |= maybeLayout
