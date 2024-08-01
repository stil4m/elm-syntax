module Elm.Parser.Layout exposing
    ( layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , maybeLayoutUntilIgnored
    , moduleLevelIndentation
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
    , positivelyIndentedPlus
    )

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Elm.Syntax.Node exposing (Node)
import Parser exposing ((|.), (|=), Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


maybeLayoutUntilIgnored : Parser () -> Parser.Parser Comments
maybeLayoutUntilIgnored end =
    let
        fromSingleLineCommentUntilEnd : Parser (Rope.Rope (Node String))
        fromSingleLineCommentUntilEnd =
            Parser.map
                (\comment ->
                    \commentsAfter ->
                        Rope.one comment |> Rope.filledPrependTo commentsAfter
                )
                Comments.singleLineCommentCore
                |= Parser.lazy (\() -> maybeLayoutUntilIgnored end)

        fromMultilineCommentNodeUntilEnd : Parser Comments
        fromMultilineCommentNodeUntilEnd =
            Parser.oneOf [ fromMultilineCommentNode, endNoComments ]

        endNoComments : Parser Comments
        endNoComments =
            positivelyIndented Rope.empty |. end

        fromCommentElseEmptyThenEnd : Parser Comments
        fromCommentElseEmptyThenEnd =
            -- since comments are comparatively rare
            -- but expensive to check for, we allow shortcutting to dead end
            Parser.andThen
                (\source ->
                    Parser.andThen
                        (\offset ->
                            case source |> String.slice offset (offset + 2) of
                                "--" ->
                                    fromSingleLineCommentUntilEnd

                                "{-" ->
                                    fromMultilineCommentNodeUntilEnd

                                _ ->
                                    endNoComments
                        )
                        Parser.getOffset
                )
                Parser.getSource

        endOrFromCommentElseEmptyThenEnd : Parser Comments
        endOrFromCommentElseEmptyThenEnd =
            Parser.oneOf
                [ endNoComments |> Parser.backtrackable
                , fromCommentElseEmptyThenEnd
                ]
    in
    Parser.oneOf
        [ whitespace
            |> Parser.andThen (\_ -> endOrFromCommentElseEmptyThenEnd)
        , endNoComments |> Parser.backtrackable
        , fromCommentElseEmptyThenEnd
        ]


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
    Parser.andThen
        (\source ->
            Parser.andThen
                (\offset ->
                    case source |> String.slice offset (offset + 2) of
                        "--" ->
                            -- this will always succeed from here, so no need to fall back to Rope.empty
                            fromSingleLineCommentNode

                        "{-" ->
                            fromMultilineCommentNodeOrEmptyOnProblem

                        _ ->
                            succeedRopeEmpty
                )
                Parser.getOffset
        )
        Parser.getSource


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
        |. positivelyIndented ()


{-| Use to check that the indentation of an already parsed token
would be valid for [`positivelyIndented`](#positivelyIndented)
-}
positivelyIndentedPlus : Int -> Parser.Parser ()
positivelyIndentedPlus extraIndent =
    Parser.andThen
        (\column ->
            Parser.andThen
                (\indent ->
                    if column > indent + extraIndent then
                        succeedUnit

                    else
                        problemPositivelyIndented
                )
                Parser.getIndent
        )
        Parser.getCol


positivelyIndented : res -> Parser.Parser res
positivelyIndented res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.getCol
        |> Parser.andThen
            (\column ->
                Parser.andThen
                    (\indent ->
                        if column > indent then
                            succeedRes

                        else
                            problemPositivelyIndented
                    )
                    Parser.getIndent
            )


succeedUnit : Parser ()
succeedUnit =
    Parser.succeed ()


problemPositivelyIndented : Parser a
problemPositivelyIndented =
    Parser.problem "must be positively indented"


optimisticLayout : Parser Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |. onTopIndentation ()


moduleLevelIndentation : res -> Parser res
moduleLevelIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.andThen
        (\column ->
            if column == 1 then
                succeedRes

            else
                problemModuleLevelIndentation
        )
        Parser.getCol


problemModuleLevelIndentation : Parser.Parser a
problemModuleLevelIndentation =
    Parser.problem "must be on module-level indentation"


onTopIndentation : res -> Parser res
onTopIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            Parser.succeed res
    in
    Parser.andThen
        (\column ->
            Parser.andThen
                (\indent ->
                    if column == indent then
                        succeedRes

                    else
                        problemTopIndentation
                )
                Parser.getIndent
        )
        Parser.getCol


problemTopIndentation : Parser.Parser a
problemTopIndentation =
    Parser.problem "must be on top indentation"


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
