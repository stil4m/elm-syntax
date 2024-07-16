module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , onTopIndentation
    , optimisticLayout
    , optimisticLayoutWith
    , positivelyIndented
    )

import Combine exposing (Parser, many1Ignore, oneOf, problem, succeed, withLocation, withState)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace exposing (many1Spaces, realNewLine)


anyComment : Combine.Parser State ()
anyComment =
    Combine.oneOf
        [ Comments.singleLineComment
        , Comments.multilineComment
        ]


layout : Parser State ()
layout =
    many1Ignore
        (oneOf
            [ anyComment
            , many1Ignore realNewLine
                |> Combine.continueWith
                    (oneOf
                        [ many1Spaces
                        , anyComment
                        ]
                    )
            , many1Spaces
            ]
        )
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent < current)
                (\stateIndent current -> "Expected indent larger than " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


optimisticLayoutWith : (() -> a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.continueWith (compute onStrict onIndented)


optimisticLayout : Parser State ()
optimisticLayout =
    Combine.manyIgnore
        (oneOf
            [ anyComment
            , Combine.many1Ignore realNewLine
                |> Combine.continueWith
                    (oneOf
                        [ many1Spaces
                        , anyComment
                        , succeed ()
                        ]
                    )
            , many1Spaces
            ]
        )


compute : (() -> a) -> (() -> Parser State a) -> Parser State a
compute onStrict onIndented =
    withLocation
        (\l ->
            if l.column == 1 then
                Combine.succeed (onStrict ())

            else
                withState
                    (\state ->
                        if List.member l.column (State.storedColumns state) then
                            Combine.succeed (onStrict ())

                        else
                            onIndented ()
                    )
        )


layoutStrict : Parser State ()
layoutStrict =
    many1Ignore
        (oneOf
            [ anyComment
            , many1Ignore realNewLine
            , many1Spaces
            ]
        )
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent == current)
                (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


onTopIndentation : Parser State ()
onTopIndentation =
    Combine.withState
        (\state ->
            Combine.withLocation
                (\{ column } ->
                    if State.currentIndent state == Just column then
                        Combine.succeed ()

                    else
                        Combine.problem "must be on top indentation"
                )
        )


positivelyIndented : Parser State ()
positivelyIndented =
    verifyIndent (\stateIndent current -> stateIndent < current) (\_ _ -> "must be positively indented")


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State ()
verifyIndent verify failMessage =
    withState
        (\state ->
            withLocation
                (\{ column } ->
                    let
                        expectedColumn : Int
                        expectedColumn =
                            State.expectedColumn state
                    in
                    if verify expectedColumn column then
                        succeed ()

                    else
                        problem (failMessage expectedColumn column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
