module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Parser as Core


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


parensPattern : Parser State (Node Pattern)
parensPattern =
    Node.parser
        (Combine.parens (Combine.sepBy "," (Layout.maybeAroundBothSides pattern))
            |> Combine.map
                (\c ->
                    case c of
                        [ x ] ->
                            ParenthesizedPattern x

                        _ ->
                            TuplePattern c
                )
        )


variablePart : Parser state (Node Pattern)
variablePart =
    Node.parserFromCore (Core.map VarPattern Tokens.functionName)


numberPart : Parser state (Node Pattern)
numberPart =
    Elm.Parser.Numbers.number IntPattern HexPattern
        |> Node.parserFromCore


charPattern : Parser state (Node Pattern)
charPattern =
    Tokens.characterLiteral
        |> Core.map CharPattern
        |> Node.parserFromCore


listPattern : Parser State (Node Pattern)
listPattern =
    Node.parser <|
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


allPattern : Parser state (Node Pattern)
allPattern =
    Core.map (\() -> AllPattern) (Core.symbol "_")
        |> Node.parserFromCore


unitPattern : Parser state (Node Pattern)
unitPattern =
    Core.map (\() -> UnitPattern) (Core.symbol "()")
        |> Node.parserFromCore


stringPattern : Parser state (Node Pattern)
stringPattern =
    Core.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Core.map StringPattern
        |> Node.parserFromCore


qualifiedPatternWithConsumeArgs : Parser State (Node Pattern)
qualifiedPatternWithConsumeArgs =
    Base.typeIndicator
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                Combine.manyWithEndLocationForLastElement range
                    Node.range
                    (qualifiedPatternArg |> Combine.ignore (Combine.maybeIgnore Layout.layout))
                    |> Combine.map
                        (\( end, args ) ->
                            Node { start = range.start, end = end }
                                (NamedPattern { moduleName = mod, name = name } args)
                        )
            )


qualifiedPatternWithoutConsumeArgs : Parser State (Node Pattern)
qualifiedPatternWithoutConsumeArgs =
    Base.typeIndicator
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.map
            (\(Node range ( mod, name )) ->
                Node range (NamedPattern { moduleName = mod, name = name } [])
            )


recordPattern : Parser State (Node Pattern)
recordPattern =
    Combine.between
        "{"
        "}"
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith (Combine.sepBy "," (Layout.maybeAroundBothSides (Node.parserFromCore Tokens.functionName)))
        )
        |> Combine.map RecordPattern
        |> Node.parser
