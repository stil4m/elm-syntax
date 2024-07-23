module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , onTopIndentation
    , optimisticLayout
    , optimisticLayoutWith
    , positivelyIndented
    )

import Combine exposing (Parser)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Parser as Core
import Set


oneOfAnyComment : List (Combine.Parser State ())
oneOfAnyComment =
    [ Comments.singleLineComment
    , Comments.multilineComment
    ]


layout : Parser State ()
layout =
    Combine.many1Ignore
        (Combine.oneOf
            ((Core.variable
                { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                , reserved = Set.empty
                , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                }
                |> Combine.fromCoreMap (\_ -> ())
             )
                :: oneOfAnyComment
            )
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
        (Combine.oneOf
            ((Core.variable
                { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                , reserved = Set.empty
                , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                }
                |> Combine.fromCoreMap (\_ -> ())
             )
                :: oneOfAnyComment
            )
        )


compute : (() -> a) -> (() -> Parser State a) -> Parser State a
compute onStrict onIndented =
    Combine.withColumn
        (\column ->
            if column == 1 then
                Combine.succeed (onStrict ())

            else
                Combine.withState
                    (\state ->
                        if List.member column (State.storedColumns state) then
                            Combine.succeed (onStrict ())

                        else
                            onIndented ()
                    )
        )


layoutStrict : Parser State ()
layoutStrict =
    Combine.many1Ignore
        (Combine.oneOf
            ((Core.variable
                { inner = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                , reserved = Set.empty
                , start = \c -> c == ' ' || c == '\n' || c == '\u{000D}'
                }
                |> Combine.fromCoreMap (\_ -> ())
             )
                :: oneOfAnyComment
            )
        )
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent == current)
                (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


onTopIndentation : Parser State ()
onTopIndentation =
    Combine.withState
        (\state ->
            Combine.withColumn
                (\column ->
                    if State.currentIndent state == Just column then
                        Combine.succeed ()

                    else
                        problemTopIndentation
                )
        )


problemTopIndentation : Combine.Parser state a
problemTopIndentation =
    Combine.problem "must be on top indentation"


positivelyIndented : Parser State ()
positivelyIndented =
    verifyIndent (\stateIndent current -> stateIndent < current) (\_ _ -> "must be positively indented")


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State ()
verifyIndent verify failMessage =
    Combine.withState
        (\state ->
            Combine.withColumn
                (\column ->
                    let
                        expectedColumn : Int
                        expectedColumn =
                            State.expectedColumn state
                    in
                    if verify expectedColumn column then
                        Combine.succeed ()

                    else
                        Combine.problem (failMessage expectedColumn column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
