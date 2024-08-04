module Elm.Parser.Layout exposing
    ( layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , maybeLayoutUntilIgnored
    , moduleLevelIndentation
    , onTopIndentation
    , optimisticLayout
    , positivelyIndentedFollowedBy
    , positivelyIndentedPlusFollowedBy
    )

import CustomParser exposing (Parser)
import CustomParser.Extra
import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Elm.Syntax.Node exposing (Node(..))
import ParserWithComments exposing (Comments, WithComments)
import Rope
import Set


maybeLayoutUntilIgnored : (String -> Parser ()) -> String -> CustomParser.Parser Comments
maybeLayoutUntilIgnored endParser endSymbol =
    whitespaceAndCommentsUntilEndComments
        (endParser endSymbol
            |> CustomParser.Extra.continueWith
                (positivelyIndentedPlusResultingIn (String.length endSymbol) Rope.empty)
        )


whitespaceAndCommentsUntilEndComments : Parser Comments -> Parser Comments
whitespaceAndCommentsUntilEndComments end =
    let
        fromSingleLineCommentUntilEnd : Parser Comments
        fromSingleLineCommentUntilEnd =
            CustomParser.map4
                (\startColumn content endLocation commentsAfter ->
                    Rope.one
                        (Node
                            { start = { row = endLocation.row, column = startColumn }
                            , end = endLocation
                            }
                            content
                        )
                        |> Rope.filledPrependTo commentsAfter
                )
                CustomParser.getCol
                Comments.singleLineCommentCore
                CustomParser.getPosition
                (CustomParser.lazy (\() -> whitespaceAndCommentsUntilEndComments end))

        fromMultilineCommentNodeUntilEnd : Parser Comments
        fromMultilineCommentNodeUntilEnd =
            Node.parserCoreMap
                (\comment ->
                    \commentsAfter ->
                        Rope.one comment |> Rope.filledPrependTo commentsAfter
                )
                Comments.multilineCommentString
                |> CustomParser.keep (CustomParser.lazy (\() -> whitespaceAndCommentsUntilEndComments end))

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
    CustomParser.oneOf
        [ whitespace
            -- whitespace can't be followed by more whitespace
            |> CustomParser.andThen (\_ -> fromCommentElseEmpty)
        , fromCommentElseEmpty
        ]


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
    CustomParser.andThen
        (\source ->
            CustomParser.andThen
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
                CustomParser.getOffset
        )
        CustomParser.getSource


succeedRopeEmpty : Parser Comments
succeedRopeEmpty =
    CustomParser.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    CustomParser.oneOf [ fromMultilineCommentNode, CustomParser.succeed Rope.empty ]


fromMultilineCommentNode : Parser Comments
fromMultilineCommentNode =
    ParserFast.map2
        (\comment commentsAfter ->
            Rope.one comment |> Rope.filledPrependTo commentsAfter
        )
        Comments.multilineCommentString
        |> CustomParser.keep whitespaceAndCommentsOrEmpty


fromSingleLineCommentNode : Parser Comments
fromSingleLineCommentNode =
    CustomParser.map4
        (\startColumn content end commentsAfter ->
            Rope.one
                (Node
                    { start = { row = end.row, column = startColumn }
                    , end = end
                    }
                    content
                )
                |> Rope.filledPrependTo commentsAfter
        )
        CustomParser.getCol
        Comments.singleLineCommentCore
        CustomParser.getPosition
        whitespaceAndCommentsOrEmpty


maybeLayout : Parser Comments
maybeLayout =
    whitespaceAndCommentsOrEmpty
        |> CustomParser.ignore (positivelyIndented ())


{-| Check that the indentation of an already parsed token
would be valid after [`maybeLayout`](#maybeLayout)
-}
positivelyIndentedPlus : Int -> CustomParser.Parser ()
positivelyIndentedPlus extraIndent =
    CustomParser.andThen
        (\column ->
            CustomParser.andThen
                (\indent ->
                    if column > indent + extraIndent then
                        succeedUnit

                    else
                        problemPositivelyIndented
                )
                CustomParser.getIndent
        )
        CustomParser.getCol


positivelyIndentedPlusResultingIn : Int -> res -> CustomParser.Parser res
positivelyIndentedPlusResultingIn extraIndent res =
    let
        succeedRes : Parser res
        succeedRes =
            CustomParser.succeed res
    in
    CustomParser.andThen
        (\column ->
            CustomParser.andThen
                (\indent ->
                    if column > indent + extraIndent then
                        succeedRes

                    else
                        problemPositivelyIndented
                )
                CustomParser.getIndent
        )
        CustomParser.getCol


positivelyIndented : res -> CustomParser.Parser res
positivelyIndented res =
    let
        succeedRes : Parser res
        succeedRes =
            CustomParser.succeed res
    in
    CustomParser.getCol
        |> CustomParser.andThen
            (\column ->
                CustomParser.andThen
                    (\indent ->
                        if column > indent then
                            succeedRes

                        else
                            problemPositivelyIndented
                    )
                    CustomParser.getIndent
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
        |> CustomParser.ignore (onTopIndentation ())


moduleLevelIndentation : res -> Parser res
moduleLevelIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            CustomParser.succeed res
    in
    CustomParser.andThen
        (\column ->
            if column == 1 then
                nextParser

            else
                problemModuleLevelIndentation
        )
        CustomParser.getCol


problemModuleLevelIndentation : CustomParser.Parser a
problemModuleLevelIndentation =
    CustomParser.problem "must be on module-level indentation"


onTopIndentation : res -> Parser res
onTopIndentation res =
    let
        succeedRes : Parser res
        succeedRes =
            CustomParser.succeed res
    in
    CustomParser.andThen
        (\column ->
            CustomParser.andThen
                (\indent ->
                    if column == indent then
                        succeedRes

                    else
                        problemTopIndentation
                )
                CustomParser.getIndent
        )
        CustomParser.getCol


problemTopIndentation : CustomParser.Parser a
problemTopIndentation =
    CustomParser.problem "must be on top indentation"


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    CustomParser.map
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
        |> CustomParser.keep x
        |> CustomParser.keep maybeLayout
