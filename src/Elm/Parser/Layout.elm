module Elm.Parser.Layout exposing (LayoutStatus(..), declarationDocumentation, layout, layoutStrict, maybeAroundBothSides, optimisticLayout, optimisticLayoutWith)

import Combine exposing (Parser, fail, many, many1, maybe, oneOf, succeed, withLocation, withState)
import Elm.Parser.Comments as Comments
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Whitespace exposing (many1Spaces, realNewLine)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node exposing (Node(..))
import List.Extra


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
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent < current))


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
                    if l.column == 1 || Just (l.column - 1) == State.currentIndent state then
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
        |> Combine.continueWith (verifyIndent (\stateIndent current -> stateIndent == current))


verifyIndent : (Int -> Int -> Bool) -> Parser State ()
verifyIndent f =
    withState
        (\s ->
            withLocation
                (\l ->
                    if f (State.expectedColumn s) l.column then
                        succeed ()

                    else
                        fail ("Expected higher indent than " ++ String.fromInt l.column)
                )
        )


maybeAroundBothSides : Parser State b -> Parser State b
maybeAroundBothSides x =
    maybe layout
        |> Combine.continueWith x
        |> Combine.ignore (maybe layout)


declarationDocumentation : Parser State (Maybe (Node Documentation))
declarationDocumentation =
    Combine.oneOf
        [ Comments.declarationDocumentation
            |> Combine.ignore layoutStrict
            |> Combine.map Just
        , Combine.withState
            (\state ->
                if State.checkParsedImportOrDeclaration state then
                    Combine.succeed Nothing

                else
                    case state |> State.getComments |> List.Extra.find (\(Node _ comment) -> String.startsWith "{-|" comment) of
                        Nothing ->
                            Combine.succeed Nothing

                        Just doc ->
                            Combine.modifyState (State.removeComment doc)
                                |> Combine.continueWith (Combine.succeed (Just doc))
            )
        ]
