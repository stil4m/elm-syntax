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


nonEmptyWhiteSpaceOrComment : Parser.Parser (Maybe (Node String))
nonEmptyWhiteSpaceOrComment =
    Parser.oneOf
        [ Parser.variable
            { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            , reserved = Set.empty
            , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            }
            |> Parser.map (\_ -> Nothing)
        , Parser.oneOf
            [ Comments.singleLineCommentCore |> Parser.getChompedString
            , Comments.multilineCommentString
            ]
            |> Node.parserCoreMap Just
        ]


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
    Parser.oneOf
        [ nonEmptyWhiteSpaceOrComment
            |> Parser.map
                (\a ->
                    Parser.Loop
                        (case a of
                            Nothing ->
                                soFar

                            Just aValue ->
                                Rope.flatFromList [ soFar, Rope.one aValue ]
                        )
                )
        , Parser.succeed (Parser.Done soFar)
        ]


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
    Parser.map
        (\head ->
            \tail ->
                case head of
                    Nothing ->
                        tail

                    Just headValue ->
                        Rope.flatFromList [ Rope.one headValue, tail ]
        )
        nonEmptyWhiteSpaceOrComment
        |= Parser.loop Rope.empty whiteSpaceAndCommentsFrom
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
