module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    ParserFast.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    ParserFast.map3
        (\x commentsAfterLeft maybeComposedWithResult ->
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
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    Parser.oneOf
        [ Parser.map
            (\() ->
                \commentsAfterAs ->
                    \( nameStartRow, nameStartColumn ) ->
                        \name ->
                            { comments = commentsAfterAs
                            , syntax =
                                PatternComposedWithAs
                                    (Node.singleLineStringFrom
                                        { row = nameStartRow, column = nameStartColumn }
                                        name
                                    )
                            }
            )
            Tokens.asToken
            |= Layout.maybeLayout
            |= Parser.getPosition
            |= Tokens.functionName
        , Parser.map
            (\() ->
                \commentsAfterCons ->
                    \patternResult ->
                        { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                        , syntax = PatternComposedWithCons patternResult.syntax
                        }
            )
            Tokens.cons
            |= Layout.maybeLayout
            |= pattern
        , Parser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    Parser.map
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
        |= Layout.maybeLayout
        -- yes, (  ) is a valid pattern but not a valid type or expression
        |= Parser.oneOf
            [ Parser.map
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
                |= Layout.maybeLayout
                |= ParserWithComments.until Tokens.parensEnd
                    (Tokens.comma |> Parser.Extra.continueWith (Layout.maybeAroundBothSides pattern))
            , Parser.map (\() -> unitPatternWithComments) Tokens.parensEnd
            ]


variablePart : Parser (WithComments Pattern)
variablePart =
    Tokens.functionName
        |> Parser.map (\var -> { comments = Rope.empty, syntax = VarPattern var })


numberPart : Parser (WithComments Pattern)
numberPart =
    Elm.Parser.Numbers.number
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })


charPattern : Parser (WithComments Pattern)
charPattern =
    Tokens.characterLiteral
        |> Parser.map (\char -> { comments = Rope.empty, syntax = CharPattern char })


listPattern : Parser (WithComments Pattern)
listPattern =
    Parser.map
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
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ Parser.map (\() -> Nothing) Tokens.squareEnd
            , Parser.map
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
                Layout.maybeLayout
                (ParserWithComments.until
                    Tokens.parensEnd
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
            )
            (ParserFast.symbol ")" { comments = Rope.empty, syntax = UnitPattern })
        )
        |> Node.parser


varPattern : Parser (WithComments (Node Pattern))
varPattern =
    Tokens.functionName
        |> ParserFast.mapWithStartAndEndPosition
            (\start var end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end } (VarPattern var)
                }
            )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    Elm.Parser.Numbers.intOrHex
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })
        |> Node.parser


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteral
        |> ParserFast.mapWithStartAndEndPosition
            (\start char end ->
                { comments = Rope.empty, syntax = Node { start = start, end = end } (CharPattern char) }
            )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    ParserFast.map2
        (\commentsBeforeElements maybeElements ->
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
        (ParserFast.symbolFollowedBy "[" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map4
                (\head commentsAfterHead tail () ->
                    Just
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                                |> Rope.prependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                Tokens.squareEnd
            )
        )
        |> Node.parser


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    ParserFast.oneOf
        [ varPattern
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


patternNotDirectlyComposing : Parser (WithComments (Node Pattern))
patternNotDirectlyComposing =
    ParserFast.oneOf
        [ varPattern
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


unitPattern : Parser (WithComments (Node Pattern))
unitPattern =
    ParserFast.symbol "()" { comments = Rope.empty, syntax = UnitPattern }
        |> Node.parser


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> Parser.map (\string -> { comments = Rope.empty, syntax = StringPattern string })


maybeDotTypeNamesTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    Parser.oneOf
        [ Parser.map
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
            |= Tokens.typeName
            |= Parser.lazy (\() -> maybeDotTypeNamesTuple)
        , Parser.succeed Nothing
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithConsumeArgs =
    Parser.map
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
        |= maybeDotTypeNamesTuple
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \arg ->
                        { comments = arg.comments |> Rope.prependTo commentsBefore
                        , syntax = arg.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= qualifiedPatternArg
            )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.orSucceed
        (ParserFast.map2
            (\startName afterStartName ->
                case afterStartName of
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
        |= maybeDotTypeNamesTuple


recordPattern : Parser (WithComments Pattern)
recordPattern =
    Parser.map
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
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ Parser.map
                (\( headStartRow, headStartEnd ) ->
                    \head ->
                        \commentsAfterHead ->
                            \tail ->
                                Just
                                    { comments =
                                        commentsAfterHead
                                            |> Rope.prependTo tail.comments
                                    , syntax =
                                        Node.singleLineStringFrom
                                            { row = headStartRow, column = headStartEnd }
                                            head
                                            :: tail.syntax
                                    }
                )
                Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayout
                |= ParserWithComments.many
                    (Parser.map
                        (\() ->
                            \beforeName ->
                                \( nameStartRow, nameStartColumn ) ->
                                    \name ->
                                        \afterName ->
                                            { comments = beforeName |> Rope.prependTo afterName
                                            , syntax =
                                                Node.singleLineStringFrom
                                                    { row = nameStartRow, column = nameStartColumn }
                                                    name
                                            }
                        )
                        Tokens.comma
                        |= Layout.maybeLayout
                        |= Parser.getPosition
                        |= Tokens.functionName
                        |= Layout.maybeLayout
                    )
                |. Tokens.curlyEnd
            , Parser.map (\() -> Nothing)
                Tokens.curlyEnd
            ]


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
