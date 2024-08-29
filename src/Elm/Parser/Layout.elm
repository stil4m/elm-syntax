module Elm.Parser.Layout exposing
    ( layoutStrict
    , layoutStrictFollowedBy
    , layoutStrictFollowedByComments
    , layoutStrictFollowedByWithComments
    , maybeAroundBothSides
    , maybeLayout
    , maybeLayoutBacktrackable
    , maybeLayoutUntilIgnored
    , maybeLayoutUntilIgnoredBacktrackable
    , moduleLevelIndentationFollowedBy
    , onTopIndentationFollowedBy
    , optimisticLayout
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlusFollowedBy
    )

import Elm.Parser.Comments as Comments
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope


maybeLayoutUntilIgnored : (String -> Comments -> Parser Comments) -> String -> Parser Comments
maybeLayoutUntilIgnored endParser endSymbol =
    whitespaceAndCommentsUntilEndComments
        (endParser endSymbol Rope.empty)
        |> endsPositivelyIndentedPlus (String.length endSymbol)


maybeLayoutUntilIgnoredBacktrackable : (String -> Comments -> Parser Comments) -> String -> Parser Comments
maybeLayoutUntilIgnoredBacktrackable endParser endSymbol =
    whitespaceAndCommentsUntilEndComments
        (endParser endSymbol Rope.empty)
        |> endsPositivelyIndentedPlusBacktrackable (String.length endSymbol)


whitespaceAndCommentsUntilEndComments : Parser Comments -> Parser Comments
whitespaceAndCommentsUntilEndComments end =
    ParserFast.chompWhileWhitespaceFollowedBy
        (ParserFast.oneOf3
            end
            (ParserFast.map2
                (\content commentsAfter ->
                    Rope.one content
                        |> Rope.filledPrependTo commentsAfter
                )
                Comments.singleLineComment
                (ParserFast.lazy (\() -> whitespaceAndCommentsUntilEndComments end))
            )
            (ParserFast.map2
                (\comment commentsAfter ->
                    Rope.one comment |> Rope.filledPrependTo commentsAfter
                )
                Comments.multilineComment
                (ParserFast.lazy (\() -> whitespaceAndCommentsUntilEndComments end))
            )
        )


whitespaceAndCommentsOrEmpty : Parser Comments
whitespaceAndCommentsOrEmpty =
    ParserFast.chompWhileWhitespaceFollowedBy
        -- whitespace can't be followed by more whitespace
        --
        -- since comments are comparatively rare
        -- but expensive to check for, we allow shortcutting
        (ParserFast.offsetSourceAndThen
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
        )


succeedRopeEmpty : Parser Comments
succeedRopeEmpty =
    ParserFast.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    ParserFast.map2OrSucceed
        (\comment commentsAfter ->
            Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        (Comments.multilineComment
            |> ParserFast.followedByChompWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop
        Rope.empty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    ParserFast.map2
        (\content commentsAfter ->
            Rope.one content |> Rope.filledPrependTo commentsAfter
        )
        (Comments.singleLineComment
            |> ParserFast.followedByChompWhileWhitespace
        )
        whitespaceAndCommentsOrEmptyLoop


whitespaceAndCommentsOrEmptyLoop : Parser Comments
whitespaceAndCommentsOrEmptyLoop =
    ParserFast.loopWhileSucceeds
        (ParserFast.oneOf2
            Comments.singleLineComment
            Comments.multilineComment
            |> ParserFast.followedByChompWhileWhitespace
        )
        Rope.empty
        (\right soFar -> soFar |> Rope.prependToFilled (Rope.one right))
        identity


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty |> endsPositivelyIndented


maybeLayoutBacktrackable : Parser Comments
maybeLayoutBacktrackable =
    whitespaceAndCommentsOrEmpty |> endsPositivelyIndentedBacktrackable


endsPositivelyIndented : Parser a -> Parser a
endsPositivelyIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> column > indent)
        "must be positively indented"
        parser


endsPositivelyIndentedBacktrackable : Parser a -> Parser a
endsPositivelyIndentedBacktrackable parser =
    ParserFast.validateEndColumnIndentationBacktrackable
        (\column indent -> column > indent)
        "must be positively indented"
        parser


endsPositivelyIndentedPlus : Int -> Parser a -> Parser a
endsPositivelyIndentedPlus extraIndent parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> column > indent + extraIndent)
        "must be positively indented"
        parser


endsPositivelyIndentedPlusBacktrackable : Int -> Parser a -> Parser a
endsPositivelyIndentedPlusBacktrackable extraIndent parser =
    ParserFast.validateEndColumnIndentationBacktrackable
        (\column indent -> column > indent + extraIndent)
        "must be positively indented"
        parser


{-| Check that the indentation of an already parsed token
would be valid after [`maybeLayout`](#maybeLayout)
-}
positivelyIndentedPlusFollowedBy : Int -> Parser a -> Parser a
positivelyIndentedPlusFollowedBy extraIndent nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column > indent + extraIndent then
                nextParser

            else
                problemPositivelyIndented
        )


positivelyIndentedFollowedBy : Parser a -> Parser a
positivelyIndentedFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column > indent then
                nextParser

            else
                problemPositivelyIndented
        )


problemPositivelyIndented : Parser a
problemPositivelyIndented =
    ParserFast.problem "must be positively indented"


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
    optimisticLayout |> endsTopIndented


moduleLevelIndentationFollowedBy : Parser a -> Parser a
moduleLevelIndentationFollowedBy nextParser =
    ParserFast.columnAndThen
        (\column ->
            if column == 1 then
                nextParser

            else
                problemModuleLevelIndentation
        )


problemModuleLevelIndentation : Parser a
problemModuleLevelIndentation =
    ParserFast.problem "must be on module-level indentation"


endsTopIndented : Parser a -> Parser a
endsTopIndented parser =
    ParserFast.validateEndColumnIndentation
        (\column indent -> column == indent + 0)
        "must be on top indentation"
        parser


onTopIndentationFollowedBy : Parser a -> Parser a
onTopIndentationFollowedBy nextParser =
    ParserFast.columnIndentAndThen
        (\column indent ->
            if column - indent == 0 then
                nextParser

            else
                problemTopIndentation
        )


problemTopIndentation : Parser a
problemTopIndentation =
    ParserFast.problem "must be on top indentation"


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    ParserFast.map3
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
