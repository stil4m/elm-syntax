module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , onTopIndentation
    , optimisticLayout
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


layoutStrict : Parser State ()
layoutStrict =
    optimisticLayout
        |> Combine.continueWith
            (verifyIndent (\stateIndent current -> stateIndent == current)
                (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)
            )


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


positivelyIndented : Parser State ()
positivelyIndented =
    verifyIndent (\stateIndent current -> stateIndent < current) (\_ _ -> "must be positively indented")


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser State ()
verifyIndent verify failMessage =
    Combine.withStateFromCore
        (\state ->
            Core.getCol
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


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    Combine.maybeIgnore layout
        |> Combine.continueWith x
        |> Combine.ignore (Combine.maybeIgnore layout)
