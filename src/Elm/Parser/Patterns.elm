module Elm.Parser.Patterns exposing (pattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    Parser.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    Parser.map
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
        |= Layout.maybeLayout
        |= maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    Parser.oneOf
        [ (Tokens.asToken
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsAfterAs ->
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
                    Layout.layout
                )
          )
            |= Parser.getPosition
            |= Tokens.functionName
        , (Tokens.cons
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsAfterCons ->
                        \patternResult ->
                            { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                            , syntax = PatternComposedWithCons patternResult.syntax
                            }
                    )
                    Layout.maybeLayout
                )
          )
            |= pattern
        , Parser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments Pattern)
parensPattern =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBeforeHead ->
                    \contentResult ->
                        { comments =
                            commentsBeforeHead
                                |> Rope.prependTo contentResult.comments
                        , syntax = contentResult.syntax
                        }
                )
                -- yes, (  ) is a valid pattern but not a valid type or expression
                Layout.maybeLayout
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
            )


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
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBeforeElements ->
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
                Layout.maybeLayout
            )
    )
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
                |= Layout.maybeLayout
                |= ParserWithComments.many
                    (Tokens.comma
                        |> Parser.Extra.continueWith
                            (Layout.maybeAroundBothSides pattern)
                    )
                |. Tokens.squareEnd
            ]


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    Parser.oneOf
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
    Parser.oneOf
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
    Parser.map (\() -> allPatternWithComments) (Parser.symbol "_")


allPatternWithComments : WithComments Pattern
allPatternWithComments =
    { comments = Rope.empty, syntax = AllPattern }


unitPattern : Parser (WithComments Pattern)
unitPattern =
    Parser.map (\() -> unitPatternWithComments) (Parser.symbol "()")


unitPatternWithComments : WithComments Pattern
unitPatternWithComments =
    { comments = Rope.empty, syntax = UnitPattern }


stringPattern : Parser (WithComments Pattern)
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> Parser.map (\string -> { comments = Rope.empty, syntax = StringPattern string })


maybeDotTypeNamesTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    Parser.oneOf
        [ (Tokens.dot
            |> Parser.Extra.continueWith
                (Parser.map
                    (\startName ->
                        \afterStartName ->
                            case afterStartName of
                                Nothing ->
                                    Just ( [], startName )

                                Just ( qualificationAfter, unqualified ) ->
                                    Just ( startName :: qualificationAfter, unqualified )
                    )
                    Tokens.typeName
                )
          )
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


qualifiedPatternWithoutConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithoutConsumeArgs =
    Parser.map
        (\firstName ->
            \after ->
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
        |= maybeDotTypeNamesTuple


recordPattern : Parser (WithComments Pattern)
recordPattern =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBeforeElements ->
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
                Layout.maybeLayout
            )
    )
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
                    ((Tokens.comma
                        |> Parser.Extra.continueWith
                            (Parser.map
                                (\beforeName ->
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
                                Layout.maybeLayout
                            )
                     )
                        |= Parser.getPosition
                        |= Tokens.functionName
                        |= Layout.maybeLayout
                    )
            , Parser.succeed Nothing
            ]
        |. Tokens.curlyEnd


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
