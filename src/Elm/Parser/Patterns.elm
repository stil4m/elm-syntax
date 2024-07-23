module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Parser as Core exposing ((|=))


tryToCompose : Node Pattern -> Parser State (Node Pattern)
tryToCompose x =
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ Core.map (\() -> \y -> Node.combine AsPattern x y)
                    (Core.keyword "as")
                    |> Combine.fromCoreIgnore Layout.layout
                    |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
                , Core.map (\() -> \y -> Node.combine UnConsPattern x y)
                    Tokens.cons
                    |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
                    |> Combine.keep pattern
                , Combine.succeed x
                ]
            )


pattern : Parser State (Node Pattern)
pattern =
    Combine.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser State (Node Pattern)
composablePatternTryToCompose =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State Pattern
parensPattern =
    Combine.between
        Tokens.parensStart
        Tokens.parensEnd
        (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
        |> Combine.map
            (\c ->
                case c of
                    [ x ] ->
                        ParenthesizedPattern x

                    _ ->
                        TuplePattern c
            )


variablePart : Parser state Pattern
variablePart =
    Core.map VarPattern Tokens.functionName
        |> Combine.fromCore


numberPart : Parser state Pattern
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> Combine.fromCore


charPattern : Parser state Pattern
charPattern =
    Tokens.characterLiteral
        |> Core.map CharPattern
        |> Combine.fromCore


listPattern : Parser State Pattern
listPattern =
    Combine.between
        Tokens.squareStart
        Tokens.squareEnd
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
            |> Combine.map ListPattern
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
        |> Core.map StringPattern
        |> Combine.fromCore


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
                (Combine.maybeIgnore Layout.layout
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
    Combine.between
        Tokens.curlyStart
        Tokens.curlyEnd
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith
                (Combine.sepBy ","
                    (Combine.maybeIgnore Layout.layout
                        |> Combine.continueWithCore
                            (Tokens.functionName |> Node.parserCore)
                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    )
                )
        )
        |> Combine.map RecordPattern
