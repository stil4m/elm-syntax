module Elm.Parser.Layout exposing
    ( layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , maybeLayoutUntilIgnored
    , moduleLevelIndentationFollowedBy
    , onTopIndentationFollowedBy
    , optimisticLayout
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlus
    )

import CustomParser exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


maybeLayoutUntilIgnored : (String -> Parser Comments -> Parser Comments) -> String -> CustomParser.Parser Comments
maybeLayoutUntilIgnored endParser endSymbol =
    whitespaceAndCommentsUntilEndComments
        (endParser endSymbol
            (positivelyIndentedPlusResultingIn (String.length endSymbol) Rope.empty)
        )


whitespaceAndCommentsUntilEndComments : Parser Comments -> Parser Comments
whitespaceAndCommentsUntilEndComments end =
    let
        fromSingleLineCommentUntilEnd : Parser Comments
        fromSingleLineCommentUntilEnd =
            CustomParser.map2
                (\content commentsAfter ->
                    Rope.one content
                        |> Rope.filledPrependTo commentsAfter
                )
                (Node.parserCore Comments.singleLineCommentCore)
                (CustomParser.lazy (\() -> whitespaceAndCommentsUntilEndComments end))

        fromMultilineCommentNodeUntilEnd : Parser Comments
        fromMultilineCommentNodeUntilEnd =
            CustomParser.map2
                (\comment commentsAfter ->
                    Rope.one comment |> Rope.filledPrependTo commentsAfter
                )
                (Node.parserCore Comments.multilineCommentString)
                (CustomParser.lazy (\() -> whitespaceAndCommentsUntilEndComments end))

        endOrFromCommentElseEmptyThenEnd : Parser Comments
        endOrFromCommentElseEmptyThenEnd =
            CustomParser.oneOf
                [ end
                , fromSingleLineCommentUntilEnd
                , fromMultilineCommentNodeUntilEnd
                ]
    in
    CustomParser.oneOf
        [ whitespace
            |> CustomParser.andThen (\_ -> endOrFromCommentElseEmptyThenEnd)
        , end
        , fromSingleLineCommentUntilEnd
        , fromMultilineCommentNodeUntilEnd
        ]


whitespaceAndCommentsOrEmpty : CustomParser.Parser Comments
whitespaceAndCommentsOrEmpty =
    CustomParser.oneOf2
        (whitespace
            -- whitespace can't be followed by more whitespace
            |> CustomParser.andThen (\_ -> fromCommentElseEmpty)
        )
        fromCommentElseEmpty


whitespaceAndCommentsOrEmpty : Parser Comments
whitespaceAndCommentsOrEmpty =
    ParserFast.oneOf2
        (whitespace
            -- whitespace can't be followed by more whitespace
            |> ParserFast.andThen (\_ -> fromCommentElseEmpty)
        )
        fromCommentElseEmpty


whitespace : Parser String
whitespace =
    CustomParser.variable
        { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        , reserved = Set.empty
        , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
        }


fromCommentElseEmpty : Parser Comments
fromCommentElseEmpty =
    -- since comments are comparatively rare
    -- but expensive to check for, we allow shortcutting to dead end
    CustomParser.offsetSourceAndThen
        (\offset source ->
            case source |> String.slice offset (offset + 2) of
                "--" ->
                    -- this will always succeed from here, so no need to fall back to Rope.empty
                    fromSingleLineCommentNode

                "{-" ->
                    fromMultilineCommentNodeOrEmptyOnProblem

                _ ->
                    succeedRopeEmpty
        )


succeedRopeEmpty : Parser Comments
succeedRopeEmpty =
    CustomParser.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    CustomParser.orSucceed fromMultilineCommentNode Rope.empty


fromMultilineCommentNode : Parser Comments
fromMultilineCommentNode =
    CustomParser.map2
        (\comment commentsAfter ->
            Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        (Node.parserCore Comments.multilineCommentString)
        whitespaceAndCommentsOrEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    CustomParser.map2
        (\content commentsAfter ->
            Rope.one content |> Rope.filledPrependTo commentsAfter
        )
        (Node.parserCore Comments.singleLineCommentCore)
        whitespaceAndCommentsOrEmpty


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty
        |> CustomParser.ignore (positivelyIndentedFollowedBy (CustomParser.succeed ()))


{-| Check that the indentation of an already parsed token
would be valid after [`maybeLayout`](#maybeLayout)
-}
positivelyIndentedPlus : Int -> CustomParser.Parser ()
positivelyIndentedPlus extraIndent =
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column > indent + extraIndent then
                succeedUnit

            else
                problemPositivelyIndented
        )


positivelyIndentedPlusResultingIn : Int -> res -> CustomParser.Parser res
positivelyIndentedPlusResultingIn extraIndent res =
    let
        succeedRes : Parser res
        succeedRes =
            CustomParser.succeed res
    in
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column > indent + extraIndent then
                succeedRes

            else
                problemPositivelyIndented
        )


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column > indent then
                nextParser

            else
                problemPositivelyIndented
        )


succeedUnit : Parser ()
succeedUnit =
    CustomParser.succeed ()


problemPositivelyIndented : Parser a
problemPositivelyIndented =
    CustomParser.problem "must be positively indented"


optimisticLayout : Parser Comments
optimisticLayout =
    whitespaceAndCommentsOrEmpty


layoutStrictFollowedByComments : Parser Comments -> Parser Comments
layoutStrictFollowedByComments nextParser =
    ParserFast.map2
        (\commentsBefore afterComments ->
            commentsBefore |> Rope.prependTo afterComments
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedByWithComments : Parser (WithComments syntax) -> Parser (WithComments syntax)
layoutStrictFollowedByWithComments nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore |> Rope.prependTo after.comments
            , syntax = after.syntax
            }
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrictFollowedBy : Parser syntax -> Parser (WithComments syntax)
layoutStrictFollowedBy nextParser =
    ParserFast.map2
        (\commentsBefore after ->
            { comments = commentsBefore, syntax = after }
        )
        optimisticLayout
        (onTopIndentationFollowedBy nextParser)


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |> CustomParser.ignore (onTopIndentationFollowedBy (CustomParser.succeed ()))


moduleLevelIndentationFollowedBy : Parser a -> Parser a
moduleLevelIndentationFollowedBy nextParser =
    CustomParser.columnAndThen
        (\column ->
            if column == 1 then
                nextParser

            else
                problemModuleLevelIndentation
        )


problemModuleLevelIndentation : CustomParser.Parser a
problemModuleLevelIndentation =
    CustomParser.problem "must be on module-level indentation"


onTopIndentationFollowedBy : Parser a -> Parser a
onTopIndentationFollowedBy nextParser =
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column == indent + 0 then
                nextParser

            else
                problemTopIndentation
        )


problemTopIndentation : CustomParser.Parser a
problemTopIndentation =
    CustomParser.problem "must be on top indentation"


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    CustomParser.map3
        (\before v after ->
            { comments =
                before
                    |> Rope.prependTo v.comments
                    |> Rope.prependTo after
            , syntax = v.syntax
            }
        )
        maybeLayout
        x
        maybeLayout
