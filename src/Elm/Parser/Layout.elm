module Elm.Parser.Layout exposing
    ( layout
    , layoutStrict
    , maybeAroundBothSides
    , onTopIndentation
    , optimisticLayout
    , optimisticLayoutWith
    , positivelyIndented
    )

import Combine exposing (Parser, fail, many1, maybe, oneOf, succeed, withLocation, withState)
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
    many1
        (oneOf
            [ anyComment
            , many1 realNewLine
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


optimisticLayoutWith : (() -> Parser State a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.continueWith (compute onStrict onIndented)


optimisticLayout : Parser State ()
optimisticLayout =
    Combine.manyIgnore
        (oneOf
            [ anyComment
            , many1 realNewLine
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


compute : (() -> Parser State a) -> (() -> Parser State a) -> Parser State a
compute onStrict onIndented =
    withState
        (\state ->
            withLocation
                (\l ->
                    if l.column == 1 || List.member l.column (State.storedColumns state) then
                        onStrict ()

                    else
                        onIndented ()
                )
        )


layoutStrict : Parser State ()
layoutStrict =
    many1
        (oneOf
            [ anyComment
            , many1 realNewLine |> Combine.continueWith (succeed ())
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
                        Combine.fail "must be on top indentation"
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
                        fail (failMessage expectedColumn column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    maybe layout
        |> Combine.continueWith x
        |> Combine.ignore (maybe layout)
