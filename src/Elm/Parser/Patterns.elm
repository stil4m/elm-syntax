module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|=))


composedWith : Parser State PatternComposedWith
composedWith =
    Layout.maybeLayout
        |> Combine.continueWith
            (Combine.oneOf
                [ Tokens.asToken
                    |> Combine.fromCoreIgnore Layout.layout
                    |> Combine.continueWithCore
                        (Node.parserCoreMap PatternComposedWithAs
                            Tokens.functionName
                        )
                , Tokens.cons
                    |> Combine.fromCoreIgnore Layout.maybeLayout
                    |> Combine.continueWith (Combine.map PatternComposedWithCons pattern)
                , Combine.succeed PatternComposedWithNothing
                ]
            )


type PatternComposedWith
    = PatternComposedWithNothing
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser State (Node Pattern)
pattern =
    Combine.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser State (Node Pattern)
composablePatternTryToCompose =
    Combine.map
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
        |> Combine.keep composedWith


parensPattern : Parser State Pattern
parensPattern =
    Combine.betweenMap
        (\c ->
            case c of
                [ x ] ->
                    ParenthesizedPattern x

                _ ->
                    TuplePattern c
        )
        Tokens.parensStart
        Tokens.parensEnd
        (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))


variablePart : Parser state Pattern
variablePart =
    Tokens.functionName
        |> Combine.fromCoreMap VarPattern


numberPart : Parser state Pattern
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> Combine.fromCore


charPattern : Parser state Pattern
charPattern =
    Tokens.characterLiteral
        |> Combine.fromCoreMap CharPattern


listPattern : Parser State Pattern
listPattern =
    Combine.betweenMap ListPattern
        Tokens.squareStart
        Tokens.squareEnd
        (Layout.maybeLayout
            |> Combine.continueWith (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
        )


composablePattern : Parser State (Node Pattern)
composablePattern =
    Combine.oneOf
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


qualifiedPatternArg : Parser State (Node Pattern)
qualifiedPatternArg =
    Combine.oneOf
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


allPattern : Parser state Pattern
allPattern =
    Combine.fromCoreMap (\() -> AllPattern) (Core.symbol "_")


unitPattern : Parser state Pattern
unitPattern =
    Combine.fromCoreMap (\() -> UnitPattern) (Core.symbol "()")


stringPattern : Parser state Pattern
stringPattern =
    Core.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Combine.fromCoreMap StringPattern


qualifiedNameRef : Core.Parser QualifiedNameRef
qualifiedNameRef =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> qualifiedNameRefHelper [] typeOrSegment)


qualifiedNameRefHelper : ModuleName -> String -> Core.Parser QualifiedNameRef
qualifiedNameRefHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ dotTypeName
            |> Core.andThen (\t -> qualifiedNameRefHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed { moduleName = List.reverse moduleNameSoFar, name = typeOrSegment })
        ]


dotTypeName : Core.Parser String
dotTypeName =
    Core.map (\() -> identity) Tokens.dot
        |= Tokens.typeName


qualifiedPatternWithConsumeArgs : Parser State Pattern
qualifiedPatternWithConsumeArgs =
    Core.map (\qualified -> \args -> NamedPattern qualified args)
        qualifiedNameRef
        |> Combine.fromCoreKeep
            (Combine.many
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.continueWith qualifiedPatternArg
                )
            )


qualifiedPatternWithoutConsumeArgs : Parser State Pattern
qualifiedPatternWithoutConsumeArgs =
    qualifiedNameRef
        |> Combine.fromCoreMap
            (\qualified -> NamedPattern qualified [])


recordPattern : Parser State Pattern
recordPattern =
    Combine.betweenMap RecordPattern
        Tokens.curlyStart
        Tokens.curlyEnd
        (Layout.maybeLayout
            |> Combine.continueWith
                (Combine.sepBy ","
                    (Layout.maybeLayout
                        |> Combine.continueWithCore
                            (Tokens.functionName |> Node.parserCore)
                        |> Combine.ignore Layout.maybeLayout
                    )
                )
        )
