module Elm.Parser.Patterns exposing (pattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|.))
import Parser.Extra
import ParserWithComments exposing (ParserWithComments)


composedWith : ParserWithComments PatternComposedWith
composedWith =
    Layout.maybeLayout
        |> ParserWithComments.continueWith
            (Core.oneOf
                [ Tokens.asToken
                    |> ParserWithComments.fromCoreIgnore Layout.layout
                    |> ParserWithComments.continueWithCore
                        (Node.parserCoreMap PatternComposedWithAs
                            Tokens.functionName
                        )
                , Tokens.cons
                    |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
                    |> ParserWithComments.continueWith (ParserWithComments.map PatternComposedWithCons pattern)
                , ParserWithComments.succeed PatternComposedWithNothing
                ]
            )


type PatternComposedWith
    = PatternComposedWithNothing
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : ParserWithComments (Node Pattern)
pattern =
    Core.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : ParserWithComments (Node Pattern)
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


parensPattern : ParserWithComments Pattern
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


variablePart : ParserWithComments Pattern
variablePart =
    Tokens.functionName
        |> ParserWithComments.fromCoreMap VarPattern


numberPart : ParserWithComments Pattern
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> ParserWithComments.fromCore


charPattern : ParserWithComments Pattern
charPattern =
    Tokens.characterLiteral
        |> ParserWithComments.fromCoreMap CharPattern


listPattern : ParserWithComments Pattern
listPattern =
    (Tokens.squareStart
        |> Parser.Extra.continueWith
            (Layout.maybeLayout
                |> ParserWithComments.continueWith (ParserWithComments.sepBy "," (Layout.maybeAroundBothSides pattern))
                |> ParserWithComments.map ListPattern
            )
    )
        |. Tokens.squareEnd


composablePattern : ParserWithComments (Node Pattern)
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


qualifiedPatternArg : ParserWithComments (Node Pattern)
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


allPattern : ParserWithComments Pattern
allPattern =
    ParserWithComments.fromCoreMap (\() -> AllPattern) (Core.symbol "_")


unitPattern : ParserWithComments Pattern
unitPattern =
    ParserWithComments.fromCoreMap (\() -> UnitPattern) (Core.symbol "()")


stringPattern : ParserWithComments Pattern
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


qualifiedPatternWithConsumeArgs : ParserWithComments Pattern
qualifiedPatternWithConsumeArgs =
    Core.map (\qualified -> \args -> NamedPattern qualified args)
        qualifiedNameRef
        |> ParserWithComments.fromCoreKeep
            (ParserWithComments.many
                (Layout.maybeLayout
                    |> Core.backtrackable
                    |> ParserWithComments.continueWith qualifiedPatternArg
                )
            )


qualifiedPatternWithoutConsumeArgs : ParserWithComments Pattern
qualifiedPatternWithoutConsumeArgs =
    qualifiedNameRef
        |> ParserWithComments.fromCoreMap
            (\qualified -> NamedPattern qualified [])


recordPattern : ParserWithComments Pattern
recordPattern =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Layout.maybeLayout
                |> ParserWithComments.continueWith
                    (ParserWithComments.sepBy ","
                        (Layout.maybeLayout
                            |> ParserWithComments.continueWithCore
                                (Tokens.functionName |> Node.parserCore)
                            |> ParserWithComments.ignore Layout.maybeLayout
                        )
                    )
                |> ParserWithComments.map RecordPattern
            )
    )
        |. Tokens.curlyEnd
