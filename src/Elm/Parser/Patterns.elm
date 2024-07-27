module Elm.Parser.Patterns exposing (pattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


composedWith : Parser PatternComposedWith
composedWith =
    Parser.map
        (\commentsBefore composedWithResult ->
            case composedWithResult of
                PatternComposedWithNothing ->
                    PatternComposedWithNothing

                PatternComposedWithAs composedWithAs ->
                    PatternComposedWithAs
                        { composedWithAs
                            | comments = Rope.flatFromList [ commentsBefore, composedWithAs.comments ]
                        }

                PatternComposedWithCons composedWithCons ->
                    PatternComposedWithCons
                        { composedWithCons
                            | comments = Rope.flatFromList [ commentsBefore, composedWithCons.comments ]
                        }
        )
        Layout.maybeLayout
        |= Parser.oneOf
            [ (Tokens.asToken
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsAfterAs ->
                            \name ->
                                PatternComposedWithAs
                                    { comments = commentsAfterAs
                                    , syntax = name
                                    }
                        )
                        Layout.layout
                    )
              )
                |= Node.parserCore Tokens.functionName
            , (Tokens.cons
                |> Parser.Extra.continueWith
                    (Parser.map
                        (\commentsAfterCons ->
                            \patternResult ->
                                PatternComposedWithCons
                                    { comments = Rope.flatFromList [ patternResult.comments, commentsAfterCons ]
                                    , syntax = patternResult.syntax
                                    }
                        )
                        Layout.maybeLayout
                    )
              )
                |= pattern
            , Parser.succeed PatternComposedWithNothing
            ]


type PatternComposedWith
    = PatternComposedWithNothing
    | PatternComposedWithAs (WithComments (Node String))
    | PatternComposedWithCons (WithComments (Node Pattern))


pattern : Parser (WithComments (Node Pattern))
pattern =
    Parser.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    Parser.map
        (\x ->
            \maybeComposedWith ->
                case maybeComposedWith of
                    PatternComposedWithNothing ->
                        x

                    PatternComposedWithAs anotherName ->
                        { comments = Rope.flatFromList [ x.comments, anotherName.comments ]
                        , syntax = Node.combine Pattern.AsPattern x.syntax anotherName.syntax
                        }

                    PatternComposedWithCons y ->
                        { comments = Rope.flatFromList [ x.comments, y.comments ]
                        , syntax = Node.combine Pattern.UnConsPattern x.syntax y.syntax
                        }
        )
        composablePattern
        |= composedWith


parensPattern : Parser (WithComments Pattern)
parensPattern =
    (Tokens.parensStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy "," (Layout.maybeAroundBothSides pattern)
                |> Parser.map
                    (\c ->
                        { comments = c.comments
                        , syntax =
                            case c.syntax of
                                [ x ] ->
                                    ParenthesizedPattern x

                                _ ->
                                    TuplePattern c.syntax
                        }
                    )
            )
    )
        |. Tokens.parensEnd


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
                                , syntax = ListPattern []
                                }

                            Just elements ->
                                { comments = Rope.flatFromList [ commentsBeforeElements, elements.comments ]
                                , syntax = ListPattern elements.syntax
                                }
                )
                Layout.maybeLayout
            )
    )
        |= Parser.oneOf
            [ Parser.map
                (\head ->
                    \commentsAfterHead ->
                        \tail ->
                            Just
                                { comments =
                                    Rope.flatFromList
                                        [ head.comments
                                        , tail.comments
                                        , commentsAfterHead
                                        ]
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
            , Parser.succeed Nothing
            ]
        |. Tokens.squareEnd


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


qualifiedNameRef : Parser.Parser QualifiedNameRef
qualifiedNameRef =
    Tokens.typeName
        |> Parser.andThen (\typeOrSegment -> qualifiedNameRefHelper [] typeOrSegment)


qualifiedNameRefHelper : ModuleName -> String -> Parser.Parser QualifiedNameRef
qualifiedNameRefHelper moduleNameSoFar typeOrSegment =
    Parser.oneOf
        [ Tokens.dot
            |> Parser.Extra.continueWith Tokens.typeName
            |> Parser.andThen (\t -> qualifiedNameRefHelper (typeOrSegment :: moduleNameSoFar) t)
        , Parser.lazy (\() -> Parser.succeed { moduleName = List.reverse moduleNameSoFar, name = typeOrSegment })
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithConsumeArgs =
    Parser.map
        (\qualified ->
            \args ->
                { comments = args.comments
                , syntax = NamedPattern qualified args.syntax
                }
        )
        qualifiedNameRef
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \arg ->
                        { comments = Rope.flatFromList [ arg.comments, commentsBefore ]
                        , syntax = arg.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= qualifiedPatternArg
            )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithoutConsumeArgs =
    qualifiedNameRef
        |> Parser.map
            (\qualified -> { comments = Rope.empty, syntax = NamedPattern qualified [] })


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
                                , syntax = RecordPattern []
                                }

                            Just elements ->
                                { comments = Rope.flatFromList [ commentsBeforeElements, elements.comments ]
                                , syntax = RecordPattern elements.syntax
                                }
                )
                Layout.maybeLayout
            )
    )
        |= Parser.oneOf
            [ Parser.map
                (\head ->
                    \commentsAfterHead ->
                        \tail ->
                            Just
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterHead
                                        , tail.comments
                                        ]
                                , syntax = head :: tail.syntax
                                }
                )
                (Node.parserCore Tokens.functionName)
                |= Layout.maybeLayout
                |= ParserWithComments.many
                    (Tokens.comma
                        |> Parser.Extra.continueWith
                            (Parser.map
                                (\beforeName name afterName ->
                                    { comments = Rope.flatFromList [ beforeName, afterName ]
                                    , syntax = name
                                    }
                                )
                                Layout.maybeLayout
                                |= Node.parserCore Tokens.functionName
                                |= Layout.maybeLayout
                            )
                    )
            , Parser.succeed Nothing
            ]
        |. Tokens.curlyEnd
