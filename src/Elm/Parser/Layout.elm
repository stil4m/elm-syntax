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
import Parser as Core exposing ((|.), (|=))
import ParserWithComments exposing (ParserWithComments)
import Rope
import Set


nonEmptyWhiteSpaceOrComment : Core.Parser (Maybe (Node String))
nonEmptyWhiteSpaceOrComment =
    Core.oneOf
        [ Core.variable
            { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            , reserved = Set.empty
            , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
            }
            |> Core.map (\_ -> Nothing)
        , Core.oneOf
            [ Comments.singleLineCommentCore |> Core.getChompedString
            , Comments.multilineCommentString
            ]
            |> Node.parserCoreMap Just
        ]


whiteSpaceAndCommentsAndAddToState : ParserWithComments ()
whiteSpaceAndCommentsAndAddToState =
    Core.loop [] whiteSpaceAndCommentsFrom
        |> Core.map
            (\newComments ->
                { comments = Rope.fromList (List.reverse newComments), syntax = () }
            )


maybeLayout : ParserWithComments ()
maybeLayout =
    whiteSpaceAndCommentsAndAddToState
        |> verifyLayoutIndent


whiteSpaceAndCommentsFrom : List (Node String) -> Core.Parser (Core.Step (List (Node String)) (List (Node String)))
whiteSpaceAndCommentsFrom soFar =
    Core.oneOf
        [ nonEmptyWhiteSpaceOrComment
            |> Core.map
                (\a ->
                    Core.Loop
                        (case a of
                            Nothing ->
                                soFar

                            Just aValue ->
                                aValue :: soFar
                        )
                )
        , Core.succeed (Core.Done soFar)
        ]


verifyLayoutIndent : ParserWithComments () -> ParserWithComments ()
verifyLayoutIndent =
    verifyIndent (\stateIndent current -> stateIndent < current)
        (\stateIndent current -> "Expected indent larger than " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


positivelyIndentedCore : Core.Parser ()
positivelyIndentedCore =
    Core.map
        (\column ->
            \indent ->
                if indent < column then
                    Core.succeed ()

                else
                    Core.problem "must be positively indented"
        )
        Core.getCol
        |= Core.getIndent
        |> Core.andThen identity


positivelyIndented : ParserWithComments ()
positivelyIndented =
    Core.map
        (\column ->
            \indent ->
                if indent < column then
                    ParserWithComments.succeed ()

                else
                    Core.problem "must be positively indented"
        )
        Core.getCol
        |= Core.getIndent
        |> Core.andThen identity


layout : ParserWithComments ()
layout =
    nonEmptyWhiteSpaceOrComment
        |> Core.andThen
            (\head ->
                Core.loop
                    (case head of
                        Nothing ->
                            []

                        Just headValue ->
                            List.singleton headValue
                    )
                    whiteSpaceAndCommentsFrom
            )
        |> Core.map
            (\newComments ->
                { comments = Rope.fromList newComments, syntax = () }
            )
        |> verifyLayoutIndent


optimisticLayout : ParserWithComments ()
optimisticLayout =
    whiteSpaceAndCommentsAndAddToState


layoutStrict : ParserWithComments ()
layoutStrict =
    optimisticLayout
        |> verifyIndent (\stateIndent current -> stateIndent == current)
            (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


onTopIndentation : res -> ParserWithComments res
onTopIndentation res =
    Core.map
        (\column ->
            \indent ->
                if indent == column then
                    ParserWithComments.succeed res

                else
                    problemTopIndentation
        )
        Core.getCol
        |= Core.getIndent
        |> Core.andThen identity


problemTopIndentation : Core.Parser a
problemTopIndentation =
    Core.problem "must be on top indentation"


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> ParserWithComments () -> ParserWithComments ()
verifyIndent verify failMessage toVerify =
    toVerify
        |. (Core.map
                (\column ->
                    \indent ->
                        if verify indent column then
                            Core.succeed ()

                        else
                            Core.problem (failMessage indent column)
                )
                Core.getCol
                |= Core.getIndent
                |> Core.andThen identity
           )


maybeAroundBothSides : ParserWithComments b -> ParserWithComments b
maybeAroundBothSides x =
    ParserWithComments.maybeIgnore layout
        |> ParserWithComments.continueWith x
        |> ParserWithComments.ignore (ParserWithComments.maybeIgnore layout)
