module Elm.Parser.DestructurePatterns exposing (destructurePattern)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.DestructurePattern as DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser as Core
import Parser.Extra


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
                , Combine.succeed PatternComposedWithNothing
                ]
            )


type PatternComposedWith
    = PatternComposedWithNothing
    | PatternComposedWithAs (Node String)


destructurePattern : Parser State (Node DestructurePattern)
destructurePattern =
    Combine.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser State (Node DestructurePattern)
composablePatternTryToCompose =
    Combine.map
        (\x ->
            \maybeComposedWith ->
                case maybeComposedWith of
                    PatternComposedWithNothing ->
                        x

                    PatternComposedWithAs anotherName ->
                        Node.combine DestructurePattern.AsPattern_ x anotherName
        )
        composablePattern
        |> Combine.keep composedWith


parensPattern : Parser State DestructurePattern
parensPattern =
    Combine.betweenMap
        (\c ->
            case c of
                [ x ] ->
                    ParenthesizedPattern_ x

                _ ->
                    TuplePattern_ c
        )
        Tokens.parensStart
        Tokens.parensEnd
        (Combine.sepBy "," (Layout.maybeAroundBothSides destructurePattern))


variablePart : Parser state DestructurePattern
variablePart =
    Tokens.functionName
        |> Combine.fromCoreMap VarPattern_


composablePattern : Parser State (Node DestructurePattern)
composablePattern =
    Combine.oneOf
        [ variablePart
        , qualifiedPatternWithConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]
        |> Node.parser


qualifiedPatternArg : Parser State (Node DestructurePattern)
qualifiedPatternArg =
    Combine.oneOf
        [ variablePart
        , qualifiedPatternWithoutConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]
        |> Node.parser


allPattern : Parser state DestructurePattern
allPattern =
    Combine.fromCoreMap (\() -> AllPattern_) (Core.symbol "_")


unitPattern : Parser state DestructurePattern
unitPattern =
    Combine.fromCoreMap (\() -> UnitPattern_) (Core.symbol "()")


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


qualifiedPatternWithConsumeArgs : Parser State DestructurePattern
qualifiedPatternWithConsumeArgs =
    Core.map (\qualified -> \args -> NamedPattern_ qualified args)
        qualifiedNameRef
        |> Combine.fromCoreKeep
            (Combine.many
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.continueWith qualifiedPatternArg
                )
            )


qualifiedPatternWithoutConsumeArgs : Parser State DestructurePattern
qualifiedPatternWithoutConsumeArgs =
    qualifiedNameRef
        |> Combine.fromCoreMap
            (\qualified -> NamedPattern_ qualified [])


recordPattern : Parser State DestructurePattern
recordPattern =
    Combine.betweenMap RecordPattern_
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
