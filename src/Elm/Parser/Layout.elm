module Elm.Parser.Layout exposing (LayoutStatus(..), layout, layoutStrict, maybeAroundBothSides, optimisticLayout, optimisticLayoutWith)

import Combine exposing (Parser, fail, many, many1, maybe, oneOf, succeed, withLocation, withState)
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


type LayoutStatus
    = Strict
    | Indented


optimisticLayoutWith : (() -> Parser State a) -> (() -> Parser State a) -> Parser State a
optimisticLayoutWith onStrict onIndented =
    optimisticLayout
        |> Combine.andThen
            (\ind ->
                case ind of
                    Strict ->
                        onStrict ()

                    Indented ->
                        onIndented ()
            )


optimisticLayout : Parser State LayoutStatus
optimisticLayout =
    many
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
        |> Combine.continueWith compute


compute : Parser State LayoutStatus
compute =
    withState
        (\state ->
            withLocation
                (\l ->
                    if l.column == 1 || List.member l.column (State.storedColumns state) then
                        succeed Strict

                    else
                        succeed Indented
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
