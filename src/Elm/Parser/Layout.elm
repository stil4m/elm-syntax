module Elm.Parser.Layout exposing (around, layout, layoutAndNewLine, layoutStrict, maybeAroundBothSides)

import Combine exposing (Parser, choice, fail, many1, maybe, or, succeed, withLocation, withState)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace exposing (many1Spaces, realNewLine)


anyComment : Combine.Parser State ()
anyComment =
    or
        Comments.singleLineComment
        Comments.multilineComment


layout : Parser State ()
layout =
    many1
        (choice
            [ anyComment
            , many1 realNewLine
                |> Combine.continueWith
                    (choice
                        [ many1Spaces |> Combine.continueWith (succeed ())
                        , anyComment
                        ]
                    )
            , many1Spaces |> Combine.continueWith (succeed ())
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent < current))
        |> Combine.continueWith (succeed ())


layoutStrict : Parser State ()
layoutStrict =
    many1
        (choice
            [ anyComment
            , many1 realNewLine |> Combine.continueWith (succeed ())
            , many1Spaces |> Combine.continueWith (succeed ())
            ]
        )
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent == current))
        |> Combine.continueWith (succeed ())


verifyIndent : (Int -> Int -> Bool) -> Parser State ()
verifyIndent f =
    withState
        (\s ->
            withLocation
                (\l ->
                    if f (State.currentIndent s) l.column then
                        succeed ()

                    else
                        fail ("Expected higher indent than " ++ String.fromInt l.column)
                )
        )


around : Parser State b -> Parser State b
around x =
    layout
        |> Combine.continueWith x
        |> Combine.ignore layout


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    maybe layout
        |> Combine.continueWith x
        |> Combine.ignore (maybe layout)


layoutAndNewLine : Combine.Parser State ()
layoutAndNewLine =
    maybe layout
        |> Combine.continueWith (many1 realNewLine)
        |> Combine.continueWith (succeed ())
