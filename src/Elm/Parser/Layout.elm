module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , maybeLayout
    , onTopIndentation
    , optimisticLayout
    , positivelyIndented
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


addAsCommentsToState : Core.Parser (List (Node String)) -> Parser State ()
addAsCommentsToState parser =
    Combine.Parser
        (\state ->
            parser
                |> Core.map
                    (\newComments ->
                        ( State.addComments newComments state, () )
                    )
        )


maybeLayout : Parser State ()
maybeLayout =
    coreWhiteSpaceAndComments
        |> addAsCommentsToState
        |> verifyLayoutIndent


coreWhiteSpaceAndComments : Core.Parser (List (Node String))
coreWhiteSpaceAndComments =
    Core.loop [] whiteSpaceAndCommentsFrom


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


positivelyIndented : Parser State ()
positivelyIndented =
    Combine.Parser
        (\state ->
            Core.getCol
                |> Core.andThen
                    (\column ->
                        let
                            expectedColumn : Int
                            expectedColumn =
                                State.expectedColumn state
                        in
                        if expectedColumn < column then
                            Core.succeed ( state, () )

                        else
                            Core.problem "must be positively indented"
                    )
        )


layout : Parser State ()
layout =
    Core.map
        (\head ->
            \tail ->
                case head of
                    Nothing ->
                        tail

                    Just headValue ->
                        headValue :: tail
        )
        nonEmptyWhiteSpaceOrComment
        |= coreWhiteSpaceAndComments
        |> addAsCommentsToState
        |> verifyLayoutIndent


optimisticLayout : Parser State ()
optimisticLayout =
    coreWhiteSpaceAndComments
        |> addAsCommentsToState


layoutStrict : Parser State ()
layoutStrict =
    optimisticLayout
        |> verifyIndent (\stateIndent current -> stateIndent == current)
            (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


onTopIndentation : res -> Parser State res
onTopIndentation res =
    Combine.withStateFromCore
        (\state ->
            Core.getCol
                |> Core.andThen
                    (\column ->
                        if State.currentIndent state == Just column then
                            Core.succeed ( state, res )

                        else
                            problemTopIndentation
                    )
        )


problemTopIndentation : Core.Parser a
problemTopIndentation =
    Core.problem "must be on top indentation"


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State () -> Parser State ()
verifyIndent verify failMessage (Combine.Parser toVerify) =
    Combine.Parser
        (\state ->
            toVerify state
                |. (Core.getCol
                        |> Core.andThen
                            (\column ->
                                let
                                    expectedColumn : Int
                                    expectedColumn =
                                        State.expectedColumn state
                                in
                                if verify expectedColumn column then
                                    Core.succeed ( state, () )

                                else
                                    Core.problem (failMessage expectedColumn column)
                            )
                   )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
