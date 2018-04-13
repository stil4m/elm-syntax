module Elm.Parser.Layout exposing (around, layout, layoutAndNewLine, layoutStrict, maybeAroundBothSides)

import Combine exposing (($>), (*>), (<*), Parser, choice, fail, many1, maybe, or, succeed, withLocation, withState)
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
                *> choice
                    [ many1Spaces $> ()
                    , anyComment
                    ]
            , many1Spaces $> ()
            ]
        )
        *> verifyIndent (\stateIndent current -> stateIndent < current)
        $> ()


layoutStrict : Parser State ()
layoutStrict =
    many1
        (choice
            [ anyComment
            , many1 realNewLine $> ()
            , many1Spaces $> ()
            ]
        )
        *> verifyIndent (\stateIndent current -> stateIndent == current)
        $> ()


verifyIndent : (Int -> Int -> Bool) -> Parser State ()
verifyIndent f =
    withState
        (\s ->
            withLocation
                (\l ->
                    if f (State.currentIndent s) l.column then
                        succeed ()
                    else
                        fail ("Expected higher indent than " ++ toString l.column)
                )
        )


around : Parser State b -> Parser State b
around x =
    layout *> x <* layout


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    maybe layout *> x <* maybe layout


layoutAndNewLine : Combine.Parser State ()
layoutAndNewLine =
    maybe layout *> many1 realNewLine $> ()
