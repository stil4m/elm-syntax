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
import Parser as Core exposing ((|.), (|=))


tryToCompose : Node Pattern -> Parser State (Node Pattern)
tryToCompose x =
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (\y -> Node.combine AsPattern x y)
                    |> Combine.ignoreEntirely (Core.keyword "as")
                    |> Combine.ignore Layout.layout
                    |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
                , Combine.succeed (\y -> Node.combine UnConsPattern x y)
                    |> Combine.ignoreEntirely Tokens.cons
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                    |> Combine.keep pattern
                , Combine.succeed x
                ]
            )


pattern : Parser State (Node Pattern)
pattern =
    Combine.lazy
        (\() ->
            composablePattern |> Combine.andThen tryToCompose
        )


parensPattern : Parser State Pattern
parensPattern =
    Combine.parens (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
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
        "["
        "]"
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
    Core.map (\() -> AllPattern) (Core.symbol "_")
        |> Combine.fromCore


unitPattern : Parser state Pattern
unitPattern =
    Core.map (\() -> UnitPattern) (Core.symbol "()")
        |> Combine.fromCore


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
        [ Core.succeed identity
            |. Tokens.dot
            |= Tokens.typeName
            |> Core.andThen (\t -> qualifiedNameRefHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed { moduleName = List.reverse moduleNameSoFar, name = typeOrSegment })
        ]


qualifiedPatternWithConsumeArgs : Parser State Pattern
qualifiedPatternWithConsumeArgs =
    Core.succeed (\qualified -> \args -> NamedPattern qualified args)
        |= qualifiedNameRef
        |> Combine.fromCore
        |> Combine.keep emptyOrFilledArgumentPatterns


emptyOrFilledArgumentPatterns : Parser State (List (Node Pattern))
emptyOrFilledArgumentPatterns =
    Combine.oneOf
        [ Core.succeed identity
            |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout |> Combine.backtrackable)
            |> Combine.keep qualifiedPatternArgumentList
        , Combine.succeed []
        ]


qualifiedPatternArgumentList : Parser State (List (Node Pattern))
qualifiedPatternArgumentList =
    Combine.succeed (\head -> \tail -> head :: tail)
        |> Combine.keep qualifiedPatternArg
        |> Combine.keep
            -- TODO remove end
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
        "{"
        "}"
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith
                (Combine.sepBy ","
                    (Layout.maybeAroundBothSides
                        (Tokens.functionName
                            |> Node.parserCore
                            |> Combine.fromCore
                        )
                    )
                )
        )
        |> Combine.map RecordPattern
