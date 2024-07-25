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

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=))
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


whiteSpaceAndCommentsAndAddToState : Parser State ()
whiteSpaceAndCommentsAndAddToState =
    Combine.Parser
        (\state ->
            Core.loop (State.getCommentsFurthestToEarliest state) whiteSpaceAndCommentsFrom
                |> Core.map
                    (\newComments ->
                        ( State.setComments newComments state, () )
                    )
        )


maybeLayout : Parser State ()
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


verifyLayoutIndent : Parser State () -> Parser State ()
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
        |= State.currentIndent
        |> Core.andThen identity


positivelyIndented : Parser State ()
positivelyIndented =
    Combine.Parser
        (\state ->
            Core.map
                (\column ->
                    \indent ->
                        if indent < column then
                            Core.succeed ( state, () )

                        else
                            Core.problem "must be positively indented"
                )
                Core.getCol
                |= State.currentIndent
                |> Core.andThen identity
        )


layout : Parser State ()
layout =
    Combine.Parser
        (\state ->
            nonEmptyWhiteSpaceOrComment
                |> Core.andThen
                    (\head ->
                        Core.loop
                            (case head of
                                Nothing ->
                                    State.getCommentsFurthestToEarliest state

                                Just headValue ->
                                    headValue :: State.getCommentsFurthestToEarliest state
                            )
                            whiteSpaceAndCommentsFrom
                    )
                |> Core.map
                    (\newComments ->
                        ( State.setComments newComments state, () )
                    )
        )
        |> verifyLayoutIndent


optimisticLayout : Parser State ()
optimisticLayout =
    whiteSpaceAndCommentsAndAddToState


layoutStrict : Parser State ()
layoutStrict =
    optimisticLayout
        |> verifyIndent (\stateIndent current -> stateIndent == current)
            (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


onTopIndentation : res -> Parser State res
onTopIndentation res =
    Combine.withStateFromCore
        (\state ->
            Core.map
                (\column ->
                    \indent ->
                        if indent == column then
                            Core.succeed ( state, res )

                        else
                            problemTopIndentation
                )
                Core.getCol
                |= State.currentIndent
                |> Core.andThen identity
        )


problemTopIndentation : Core.Parser a
problemTopIndentation =
    Core.problem "must be on top indentation"


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State () -> Parser State ()
verifyIndent verify failMessage (Combine.Parser toVerify) =
    Combine.Parser
        (\state ->
            toVerify state
                |. (Core.map
                        (\column ->
                            \indent ->
                                if verify indent column then
                                    Core.succeed ()

                                else
                                    Core.problem (failMessage indent column)
                        )
                        Core.getCol
                        |= State.currentIndent
                        |> Core.andThen identity
                   )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
