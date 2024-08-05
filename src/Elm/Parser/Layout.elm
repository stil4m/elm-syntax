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
    CustomParser.oneOf
        [ whitespace
            -- whitespace can't be followed by more whitespace
            |> CustomParser.andThen (\_ -> fromCommentElseEmpty)
        , fromCommentElseEmpty
        ]


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


succeedRopeEmpty : Parser (Rope.Rope a)
succeedRopeEmpty =
    CustomParser.succeed Rope.empty


fromMultilineCommentNodeOrEmptyOnProblem : Parser Comments
fromMultilineCommentNodeOrEmptyOnProblem =
    CustomParser.oneOf [ fromMultilineCommentNode, CustomParser.succeed Rope.empty ]


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
        |> CustomParser.ignore positivelyIndented


{-| Use to check that the indentation of an already parsed token
would be valid for [`positivelyIndented`](#positivelyIndented)
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


positivelyIndented : CustomParser.Parser ()
positivelyIndented =
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column > indent then
                succeedUnit

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


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |> CustomParser.ignore onTopIndentation


moduleLevelIndentation : Parser ()
moduleLevelIndentation =
    CustomParser.columnAndThen
        (\column ->
            if column == 1 then
                succeedUnit

            else
                problemModuleLevelIndentation
        )


problemModuleLevelIndentation : CustomParser.Parser a
problemModuleLevelIndentation =
    CustomParser.problem "must be on module-level indentation"


onTopIndentation : Parser ()
onTopIndentation =
    CustomParser.columnIndentAndThen
        (\column indent ->
            if column == indent then
                succeedUnit

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
