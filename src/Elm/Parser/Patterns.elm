module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import CustomParser exposing (Parser)
import CustomParser.Extra
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    CustomParser.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    CustomParser.map
        (\x ->
            \commentsAfterLeft ->
                \maybeComposedWithResult ->
                    { comments =
                        x.comments
                            |> Rope.prependTo commentsAfterLeft
                            |> Rope.prependTo maybeComposedWithResult.comments
                    , syntax =
                        case maybeComposedWithResult.syntax of
                            PatternComposedWithNothing () ->
                                x.syntax

                    PatternComposedWithAs anotherName ->
                        Node.combine Pattern.AsPattern x.syntax anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern x.syntax y
            }
        )
        composablePattern
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    CustomParser.oneOf
        [ CustomParser.map4
            (\() ->
                \commentsAfterAs ->
                    \nameStart ->
                        \name ->
                            { comments = commentsAfterAs
                            , syntax =
                                PatternComposedWithAs
                                    (Node.singleLineStringFrom nameStart
                                        name
                                    )
                            }
            )
            Tokens.asToken
            Layout.maybeLayout
            CustomParser.getPosition
            Tokens.functionName
        , CustomParser.map
            (\() ->
                \commentsAfterCons ->
                    \patternResult ->
                        { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                        , syntax = PatternComposedWithCons patternResult.syntax
                        }
            )
            Tokens.cons
            |> CustomParser.keep Layout.maybeLayout
            |> CustomParser.keep pattern
        , CustomParser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    CustomParser.map
        (\() ->
            \commentsBeforeHead ->
                \contentResult ->
                    { comments =
                        commentsBeforeHead
                            |> Rope.prependTo contentResult.comments
                    , syntax = contentResult.syntax
                    }
        )
        Tokens.parensStart
        |> CustomParser.keep Layout.maybeLayout
        -- yes, (  ) is a valid pattern but not a valid type or expression
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\headResult ->
                        \commentsAfterHead ->
                            \tailResult ->
                                { comments =
                                    headResult.comments
                                        |> Rope.prependTo commentsAfterHead
                                        |> Rope.prependTo tailResult.comments
                                , syntax =
                                    case tailResult.syntax of
                                        [] ->
                                            ParenthesizedPattern headResult.syntax

                                        _ ->
                                            TuplePattern (headResult.syntax :: tailResult.syntax)
                                }
                    )
                    pattern
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep
                        (ParserWithComments.until
                            Tokens.parensEnd
                            (Tokens.comma |> CustomParser.Extra.continueWith (Layout.maybeAroundBothSides pattern))
                        )
                , CustomParser.map (\() -> unitPatternWithComments) Tokens.parensEnd
                ]
            )


variablePart : Parser (WithComments Pattern)
variablePart =
    Tokens.functionName
        |> CustomParser.map (\var -> { comments = Rope.empty, syntax = VarPattern var })


numberPart : Parser (WithComments Pattern)
numberPart =
    Elm.Parser.Numbers.number
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })


charPattern : Parser (WithComments Pattern)
charPattern =
    Tokens.characterLiteral
        |> CustomParser.map (\char -> { comments = Rope.empty, syntax = CharPattern char })


listPattern : Parser (WithComments Pattern)
listPattern =
    CustomParser.map
        (\() ->
            \commentsBeforeElements ->
                \maybeElements ->
                    case maybeElements of
                        Nothing ->
                            { comments = commentsBeforeElements
                            , syntax = patternListEmpty
                            }

                        Just elements ->
                            { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                            , syntax = ListPattern elements.syntax
                            }
        )
        Tokens.squareStart
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map (\() -> Nothing) Tokens.squareEnd
                , CustomParser.map
                    (\head ->
                        \commentsAfterHead ->
                            \tail ->
                                Just
                                    { comments =
                                        head.comments
                                            |> Rope.prependTo tail.comments
                                            |> Rope.prependTo commentsAfterHead
                                    , syntax = head.syntax :: tail.syntax
                                    }
                    )
                    pattern
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep
                        (ParserWithComments.many
                            (Tokens.comma
                                |> CustomParser.Extra.continueWith
                                    (Layout.maybeAroundBothSides pattern)
                            )
                        )
                    |> CustomParser.ignore Tokens.squareEnd
                ]
            )


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    CustomParser.oneOf
        [ variablePart
        , qualifiedPatternWithConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


qualifiedPatternArg : Parser (WithComments (Node Pattern))
qualifiedPatternArg =
    CustomParser.oneOf
        [ variablePart
        , qualifiedPatternWithoutConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        , stringPattern
        , listPattern
        , numberPart
        , charPattern
        ]


allPattern : Parser (WithComments (Node Pattern))
allPattern =
    ParserFast.symbol "_" { comments = Rope.empty, syntax = AllPattern }
        |> Node.parser


allPattern : Parser (WithComments Pattern)
allPattern =
    CustomParser.map (\() -> allPatternWithComments) (CustomParser.symbol "_")


allPatternWithComments : WithComments Pattern
allPatternWithComments =
    { comments = Rope.empty, syntax = AllPattern }


unitPattern : Parser (WithComments Pattern)
unitPattern =
    CustomParser.map (\() -> unitPatternWithComments) (CustomParser.symbol "()")


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> CustomParser.map (\string -> { comments = Rope.empty, syntax = StringPattern string })


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.oneOf
        [ CustomParser.map
            (\() ->
                \startName ->
                    \afterStartName ->
                        case afterStartName of
                            Nothing ->
                                Just ( [], startName )

                            Just ( qualificationAfter, unqualified ) ->
                                Just ( startName :: qualificationAfter, unqualified )
            )
            Tokens.dot
            |> CustomParser.keep Tokens.typeName
            |> CustomParser.keep (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        , CustomParser.succeed Nothing
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithConsumeArgs =
    CustomParser.map
        (\startName ->
            \afterStartName ->
                \args ->
                    { comments = args.comments
                    , syntax =
                        NamedPattern
                            (case afterStartName of
                                Nothing ->
                                    { moduleName = [], name = startName }

                                Just ( qualificationAfter, unqualified ) ->
                                    { moduleName = startName :: qualificationAfter, name = unqualified }
                            )
                            args.syntax
                    }
        )
        Tokens.typeName
        |> CustomParser.keep maybeDotTypeNamesTuple
        |> CustomParser.keep
            (ParserWithComments.many
                (CustomParser.map
                    (\commentsBefore ->
                        \arg ->
                            { comments = arg.comments |> Rope.prependTo commentsBefore
                            , syntax = arg.syntax
                            }
                    )
                    (Layout.maybeLayout |> CustomParser.backtrackable)
                    |> CustomParser.keep qualifiedPatternArg
                )
            )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithoutConsumeArgs =
    CustomParser.map
        (\firstName ->
            \after ->
                case after of
                    Nothing ->
                        Just ( [], startName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( startName :: qualificationAfter, unqualified )
            )
            (ParserFast.symbolFollowedBy "." Tokens.typeName)
            (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        )
        Nothing


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\startName afterStartName args ->
            { comments = args.comments
            , syntax =
                NamedPattern
                    (case afterStartName of
                        Nothing ->
                            { moduleName = [], name = startName }

                        Just ( qualificationAfter, unqualified ) ->
                            { moduleName = startName :: qualificationAfter, name = unqualified }
                    )
                    args.syntax
            }
        )
        Tokens.typeName
        |> CustomParser.keep maybeDotTypeNamesTuple


recordPattern : Parser (WithComments Pattern)
recordPattern =
    CustomParser.map
        (\() ->
            \commentsBeforeElements ->
                \maybeElements ->
                    case maybeElements of
                        Nothing ->
                            { comments = commentsBeforeElements
                            , syntax = patternRecordEmpty
                            }

                        Just elements ->
                            { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                            , syntax = RecordPattern elements.syntax
                            }
        )
        Tokens.curlyStart
        |> CustomParser.keep Layout.maybeLayout
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
                    (\headStart ->
                        \head ->
                            \commentsAfterHead ->
                                \tail ->
                                    Just
                                        { comments =
                                            commentsAfterHead
                                                |> Rope.prependTo tail.comments
                                        , syntax =
                                            Node.singleLineStringFrom
                                                headStart
                                                head
                                                :: tail.syntax
                                        }
                    )
                    CustomParser.getPosition
                    |> CustomParser.keep Tokens.functionName
                    |> CustomParser.keep Layout.maybeLayout
                    |> CustomParser.keep
                        (ParserWithComments.many
                            (CustomParser.map
                                (\() ->
                                    \beforeName ->
                                        \nameStart ->
                                            \name ->
                                                \afterName ->
                                                    { comments = beforeName |> Rope.prependTo afterName
                                                    , syntax =
                                                        Node.singleLineStringFrom nameStart
                                                            name
                                                    }
                                )
                                Tokens.comma
                                |> CustomParser.keep Layout.maybeLayout
                                |> CustomParser.keep CustomParser.getPosition
                                |> CustomParser.keep Tokens.functionName
                                |> CustomParser.keep Layout.maybeLayout
                            )
                        )
                    |> CustomParser.ignore Tokens.curlyEnd
                , CustomParser.map (\() -> Nothing)
                    Tokens.curlyEnd
                ]
            )


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
