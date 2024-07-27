module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
    , positivelyIndentedCore
    )

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Elm.Syntax.Node exposing (Node)
import Parser exposing ((|.), (|=), Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope exposing (Rope)
import Set


{-| Nothing if there can't be more whitespace/comments ahead,
otherwise Just with the next comment node
-}
maybeNonEmptyWhiteSpaceAndNextComment : Parser.Parser (Maybe (Node String))
maybeNonEmptyWhiteSpaceAndNextComment =
    Parser.oneOf
        [ Parser.variable
            { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            , reserved = Set.empty
            , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            }
            -- whitespace can't be followed by more whitespace
            |> Parser.andThen (\_ -> commentOrNothing)
        , -- below will never run with elm-format-ed code
          commentOrNothing
        ]


commentOrNothing : Parser (Maybe (Node String))
commentOrNothing =
    -- since comments are comparatively rare
    -- but expensive to check for, we allow shortcutting to dead end
    Parser.map
        (\source offset ->
            case source |> String.slice offset (offset + 2) of
                "--" ->
                    -- this will always succeed from here, so no need to fall back to Nothing
                    justSingleLineCommentNode

                "{-" ->
                    justMultilineCommentNodeOrNothingOnProblem

                _ ->
                    succeedNothing
        )
        Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen identity


succeedNothing : Parser (Maybe a)
succeedNothing =
    Parser.succeed Nothing


justMultilineCommentNodeOrNothingOnProblem : Parser (Maybe (Node String))
justMultilineCommentNodeOrNothingOnProblem =
    Parser.oneOf [ justMultilineCommentNode, Parser.succeed Nothing ]


justMultilineCommentNode : Parser (Maybe (Node String))
justMultilineCommentNode =
    Comments.multilineCommentString
        |> Node.parserCoreMap Just


justSingleLineCommentNode : Parser (Maybe (Node String))
justSingleLineCommentNode =
    Comments.singleLineCommentCore
        |> Parser.getChompedString
        |> Node.parserCoreMap Just


whiteSpaceAndComments : Parser Comments
whiteSpaceAndComments =
    Parser.oneOf
        [ Parser.keyword " "
            |> Parser.andThen (\() -> whiteSpaceAndCommentsLoop)
        , Parser.token " "
            |> Parser.map (\() -> Rope.empty)

        -- fallback if the hacky shortcuts don't commit
        , whiteSpaceAndCommentsLoop
        ]


whiteSpaceAndCommentsLoop : Parser Comments
whiteSpaceAndCommentsLoop =
    Parser.loop Rope.empty whiteSpaceAndCommentsFrom


maybeLayout : Parser Comments
maybeLayout =
    whiteSpaceAndComments
        |. verifyLayoutIndent


whiteSpaceAndCommentsFrom : Rope (Node String) -> Parser.Parser (Parser.Step (Rope (Node String)) Comments)
whiteSpaceAndCommentsFrom soFar =
    maybeNonEmptyWhiteSpaceAndNextComment
        |> Parser.map
            (\a ->
                case a of
                    Nothing ->
                        Parser.Done soFar

                    Just aValue ->
                        Parser.Loop (Rope.flatFromList [ soFar, Rope.one aValue ])
            )


verifyLayoutIndent : Parser ()
verifyLayoutIndent =
    verifyIndent (\stateIndent current -> stateIndent < current)
        (\stateIndent current -> "Expected indent larger than " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


positivelyIndentedCore : Parser.Parser ()
positivelyIndentedCore =
    Parser.map
        (\column ->
            \indent ->
                if indent < column then
                    Parser.succeed ()

                else
                    Parser.problem "must be positively indented"
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


positivelyIndented : res -> Parser res
positivelyIndented res =
    Parser.map
        (\column ->
            \indent ->
                if indent < column then
                    Parser.succeed res

                else
                    Parser.problem "must be positively indented"
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


layout : Parser Comments
layout =
    (maybeNonEmptyWhiteSpaceAndNextComment
        |> Parser.andThen
            (\head ->
                case head of
                    Nothing ->
                        Parser.succeed Rope.empty

                    Just headValue ->
                        Parser.map (\tail -> Rope.flatFromList [ Rope.one headValue, tail ])
                            (Parser.loop Rope.empty whiteSpaceAndCommentsFrom)
            )
    )
        |. verifyLayoutIndent


optimisticLayout : Parser Comments
optimisticLayout =
    whiteSpaceAndComments


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
                    Parser.succeed ()

                else
                    Parser.problem (failMessage indent column)
        )
        Parser.getCol
        |= Parser.getIndent
        |> Parser.andThen identity


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
