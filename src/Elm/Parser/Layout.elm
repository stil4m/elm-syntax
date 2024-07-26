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

import Elm.Parser.Comments as Comments
import Elm.Parser.Node as Node
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing ((|.), (|=), Parser)
import ParserWithComments exposing (Comments, WithComments)
import Rope
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


whiteSpaceAndComments : Parser Comments
whiteSpaceAndComments =
    Core.loop [] whiteSpaceAndCommentsFrom


maybeLayout : Parser Comments
maybeLayout =
    whiteSpaceAndComments
        |> verifyLayoutIndent


whiteSpaceAndCommentsFrom : List (Node String) -> Core.Parser (Core.Step (List (Node String)) Comments)
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
        , Core.succeed (Core.Done (Rope.fromList (List.reverse soFar)))
        ]


verifyLayoutIndent : Parser Comments -> Parser Comments
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
        |= Core.getIndent
        |> Core.andThen identity


positivelyIndented : Parser Comments
positivelyIndented =
    Core.map
        (\column ->
            \indent ->
                if indent < column then
                    Core.succeed Rope.empty

                else
                    Core.problem "must be positively indented"
        )
        Core.getCol
        |= Core.getIndent
        |> Core.andThen identity


layout : Parser Comments
layout =
    Core.map
        (\head ->
            \tail ->
                case head of
                    Nothing ->
                        tail

                    Just headValue ->
                        Rope.one headValue |> Rope.prependTo tail
        )
        nonEmptyWhiteSpaceOrComment
        |= Core.loop [] whiteSpaceAndCommentsFrom
        |> verifyLayoutIndent


optimisticLayout : Parser Comments
optimisticLayout =
    whiteSpaceAndComments


layoutStrict : Parser Comments
layoutStrict =
    optimisticLayout
        |> verifyIndent (\stateIndent current -> stateIndent == current)
            (\stateIndent current -> "Expected indent " ++ String.fromInt stateIndent ++ ", got " ++ String.fromInt current)


onTopIndentation : res -> Parser res
onTopIndentation res =
    Core.map
        (\column ->
            \indent ->
                if indent == column then
                    Core.succeed res

                else
                    problemTopIndentation
        )
        Core.getCol
        |= Core.getIndent
        |> Core.andThen identity


problemTopIndentation : Core.Parser a
problemTopIndentation =
    Core.problem "must be on top indentation"


verifyIndent : (Int -> Int -> Bool) -> (Int -> Int -> String) -> Parser Comments -> Parser Comments
verifyIndent verify failMessage toVerify =
    toVerify
        |. (Core.map
                (\column ->
                    \indent ->
                        if verify indent column then
                            Core.succeed ()

                        else
                            Core.problem (failMessage indent column)
                )
                Core.getCol
                |= Core.getIndent
                |> Core.andThen identity
           )


maybeAroundBothSides : Parser (WithComments b) -> Parser (WithComments b)
maybeAroundBothSides x =
    Core.map
        (\before ->
            \v ->
                \after ->
                    { comments = Rope.flatFromList [ before, v.comments, after ]
                    , syntax = v.syntax
                    }
        )
        maybeLayout
        |= x
        |= maybeLayout
