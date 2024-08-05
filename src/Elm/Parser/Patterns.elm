module Elm.Parser.Patterns exposing (pattern)

import CustomParser exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node)
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
    CustomParser.map3
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
    CustomParser.oneOf
        [ CustomParser.map3
            (\commentsAfterAs nameStart name ->
                { comments = commentsAfterAs
                , syntax =
                    PatternComposedWithAs
                        (Node.singleLineStringFrom nameStart
                            name
                        )
                }
            )
            (CustomParser.keywordFollowedBy "as" Layout.maybeLayout)
            CustomParser.getPosition
            Tokens.functionName
        , CustomParser.map2
            (\commentsAfterCons patternResult ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                , syntax = PatternComposedWithCons patternResult.syntax
                }
            )
            (CustomParser.symbolFollowedBy "::" Layout.maybeLayout)
            pattern
        , CustomParser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments Pattern)
parensPattern =
    CustomParser.map2
        (\commentsBeforeHead contentResult ->
            { comments =
                commentsBeforeHead
                    |> Rope.prependTo contentResult.comments
            , syntax = contentResult.syntax
            }
        )
        (CustomParser.symbolFollowedBy "(" Layout.maybeLayout)
        -- yes, (  ) is a valid pattern but not a valid type or expression
        (CustomParser.oneOf
            [ CustomParser.map3
                (\headResult commentsAfterHead tailResult ->
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
                Layout.maybeLayout
                (ParserWithComments.until
                    Tokens.parensEnd
                    (CustomParser.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
            , CustomParser.symbol ")" { comments = Rope.empty, syntax = UnitPattern }
            ]
        )


variablePart : Parser (WithComments Pattern)
variablePart =
    Tokens.functionName
        |> CustomParser.map (\var -> { comments = Rope.empty, syntax = VarPattern var })


numberPart : Parser (WithComments Pattern)
numberPart =
    Elm.Parser.Numbers.intOrHex
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })


charPattern : Parser (WithComments Pattern)
charPattern =
    Tokens.characterLiteral
        |> CustomParser.map (\char -> { comments = Rope.empty, syntax = CharPattern char })


listPattern : Parser (WithComments Pattern)
listPattern =
    CustomParser.map2
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
        (CustomParser.symbolFollowedBy "[" Layout.maybeLayout)
        (CustomParser.oneOf
            [ CustomParser.symbol "]" Nothing
            , CustomParser.map4
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
                    (CustomParser.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                Tokens.squareEnd
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
        |> Node.parser


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
        |> Node.parser


allPattern : Parser (WithComments Pattern)
allPattern =
    CustomParser.symbol "_" { comments = Rope.empty, syntax = AllPattern }


unitPattern : Parser (WithComments Pattern)
unitPattern =
    CustomParser.symbol "()" { comments = Rope.empty, syntax = UnitPattern }


stringPattern : Parser (WithComments Pattern)
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> CustomParser.map (\string -> { comments = Rope.empty, syntax = StringPattern string })


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.oneOf
        [ CustomParser.map2
            (\startName afterStartName ->
                case afterStartName of
                    Nothing ->
                        Just ( [], startName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( startName :: qualificationAfter, unqualified )
            )
            (CustomParser.symbolFollowedBy "." Tokens.typeName)
            (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        , CustomParser.succeed Nothing
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithConsumeArgs =
    CustomParser.map3
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
        maybeDotTypeNamesTuple
        (ParserWithComments.many
            (CustomParser.map2
                (\commentsBefore arg ->
                    { comments = arg.comments |> Rope.prependTo commentsBefore
                    , syntax = arg.syntax
                    }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                qualifiedPatternArg
            )
        )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithoutConsumeArgs =
    CustomParser.map2
        (\firstName after ->
            case after of
                Nothing ->
                    { comments = Rope.empty
                    , syntax = NamedPattern { moduleName = [], name = firstName } []
                    }

                Just ( qualificationAfter, unqualified ) ->
                    { comments = Rope.empty
                    , syntax = NamedPattern { moduleName = firstName :: qualificationAfter, name = unqualified } []
                    }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


recordPattern : Parser (WithComments Pattern)
recordPattern =
    CustomParser.map2
        (\commentsBeforeElements maybeElements ->
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
        (CustomParser.symbolFollowedBy "{" Layout.maybeLayout)
        (CustomParser.oneOf
            [ CustomParser.map5
                (\headStart head commentsAfterHead tail () ->
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
                Tokens.functionName
                Layout.maybeLayout
                (ParserWithComments.many
                    (CustomParser.map4
                        (\beforeName nameStart name afterName ->
                            { comments = beforeName |> Rope.prependTo afterName
                            , syntax =
                                Node.singleLineStringFrom nameStart
                                    name
                            }
                        )
                        (CustomParser.symbolFollowedBy "," Layout.maybeLayout)
                        CustomParser.getPosition
                        Tokens.functionName
                        Layout.maybeLayout
                    )
                )
                Tokens.curlyEnd
            , CustomParser.symbol "}" Nothing
            ]
        )


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
