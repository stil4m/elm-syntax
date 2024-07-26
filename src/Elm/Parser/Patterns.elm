module Elm.Parser.Patterns exposing (pattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


composedWith : Parser (WithComments PatternComposedWith)
composedWith =
    Core.map
        (\commentsBefore composedWithResult ->
            { comments = Rope.flatFromList [ commentsBefore, composedWithResult.comments ]
            , syntax = composedWithResult.syntax
            }
        )
        Layout.maybeLayout
        |= Core.oneOf
            [ (Tokens.asToken
                |> Parser.Extra.continueWith
                    (Core.map
                        (\commentsAfterAs ->
                            \name ->
                                { comments = commentsAfterAs
                                , syntax = PatternComposedWithAs name
                                }
                        )
                        Layout.layout
                    )
              )
                |= Node.parserCore Tokens.functionName
            , (Tokens.cons
                |> Parser.Extra.continueWith
                    (Core.map
                        (\commentsAfterCons ->
                            \patternResult ->
                                { comments = Rope.flatFromList [ patternResult.comments, commentsAfterCons ]
                                , syntax = PatternComposedWithCons patternResult.syntax
                                }
                        )
                        Layout.maybeLayout
                    )
              )
                |= pattern
            , ParserWithComments.succeed PatternComposedWithNothing
            ]


type PatternComposedWith
    = PatternComposedWithNothing
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    Core.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    ParserWithComments.map
        (\x ->
            \maybeComposedWith ->
                case maybeComposedWith of
                    PatternComposedWithNothing ->
                        x

                    PatternComposedWithAs anotherName ->
                        Node.combine Pattern.AsPattern x anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern x y
        )
        composablePattern
        |> ParserWithComments.keep composedWith


parensPattern : Parser (WithComments Pattern)
parensPattern =
    (Tokens.parensStart
        |> Parser.Extra.continueWith
            (ParserWithComments.sepBy "," (Layout.maybeAroundBothSides pattern)
                |> ParserWithComments.map
                    (\c ->
                        case c of
                            [ x ] ->
                                ParenthesizedPattern x

                            _ ->
                                TuplePattern c
                    )
            )
    )
        |. Tokens.parensEnd


variablePart : Parser (WithComments Pattern)
variablePart =
    Tokens.functionName
        |> ParserWithComments.fromCoreMap VarPattern


numberPart : Parser (WithComments Pattern)
numberPart =
    Elm.Parser.Numbers.number
        (\n -> { comments = Rope.empty, syntax = IntPattern n })
        (\n -> { comments = Rope.empty, syntax = HexPattern n })


charPattern : Parser (WithComments Pattern)
charPattern =
    Tokens.characterLiteral
        |> ParserWithComments.fromCoreMap CharPattern


listPattern : Parser (WithComments Pattern)
listPattern =
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBeforeElements ->
                    \elements ->
                        { comments = Rope.flatFromList [ commentsBeforeElements, elements.comments ]
                        , syntax = ListPattern elements.syntax
                        }
                )
                Layout.maybeLayout
            )
    )
        |= Core.oneOf
            [ Core.map
                (\head ->
                    \commentsAfterHead ->
                        \tail ->
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
            , ParserWithComments.succeed []
            ]
        |. Tokens.squareEnd


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    Core.oneOf
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
    Core.oneOf
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
    ParserWithComments.fromCoreMap (\() -> AllPattern) (Core.symbol "_")


unitPattern : Parser (WithComments Pattern)
unitPattern =
    ParserWithComments.fromCoreMap (\() -> UnitPattern) (Core.symbol "()")


stringPattern : Parser (WithComments Pattern)
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteral
        |> ParserWithComments.fromCoreMap StringPattern


qualifiedNameRef : Core.Parser QualifiedNameRef
qualifiedNameRef =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> qualifiedNameRefHelper [] typeOrSegment)


qualifiedNameRefHelper : ModuleName -> String -> Core.Parser QualifiedNameRef
qualifiedNameRefHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ Tokens.dot
            |> Parser.Extra.continueWith Tokens.typeName
            |> Core.andThen (\t -> qualifiedNameRefHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed { moduleName = List.reverse moduleNameSoFar, name = typeOrSegment })
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithConsumeArgs =
    Core.map
        (\qualified ->
            \args ->
                { comments = args.comments
                , syntax = NamedPattern qualified args.syntax
                }
        )
        qualifiedNameRef
        |= ParserWithComments.many
            (Core.map
                (\commentsBefore ->
                    \arg ->
                        { comments = Rope.flatFromList [ arg.comments, commentsBefore ]
                        , syntax = arg.syntax
                        }
                )
                (Layout.maybeLayout |> Core.backtrackable)
                |= qualifiedPatternArg
            )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments Pattern)
qualifiedPatternWithoutConsumeArgs =
    qualifiedNameRef
        |> ParserWithComments.fromCoreMap
            (\qualified -> NamedPattern qualified [])


recordPattern : Parser (WithComments Pattern)
recordPattern =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBeforeElements ->
                    \elements ->
                        { comments = Rope.flatFromList [ commentsBeforeElements, elements.comments ]
                        , syntax = RecordPattern elements.syntax
                        }
                )
                Layout.maybeLayout
            )
    )
        |= Core.oneOf
            [ Core.map
                (\head ->
                    \commentsAfterHead ->
                        \tail ->
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
                            (Core.map
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
            , ParserWithComments.succeed []
            ]
        |. Tokens.curlyEnd
