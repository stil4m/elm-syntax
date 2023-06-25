module Elm.Parser.DestructurePatterns exposing (destructurePattern)

import Combine exposing (Parser)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Parser as Core exposing ((|.))


tryToCompose : Node DestructurePattern -> Parser State (Node DestructurePattern)
tryToCompose x =
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (\y -> Node.combine AsPattern_ x y)
                    |> Combine.ignore
                        (Core.keyword "as"
                            |> Combine.ignoreFromCore Layout.layout
                        )
                    |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
                , Combine.succeed x
                ]
            )


destructurePattern : Parser State (Node DestructurePattern)
destructurePattern =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State (Node DestructurePattern)
parensPattern =
    Combine.lazy
        (\() ->
            Node.parser
                (Combine.parens (Combine.sepBy "," (Layout.maybeAroundBothSides destructurePattern))
                    |> Combine.map
                        (\c ->
                            case c of
                                [ x ] ->
                                    ParenthesizedPattern_ x

                                _ ->
                                    TuplePattern_ c
                        )
                )
        )


variablePart : Parser State (Node DestructurePattern)
variablePart =
    Node.parserCore (Core.map VarPattern_ Tokens.functionName)
        |> Combine.fromCore


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Node DestructurePattern)
composablePattern =
    Combine.oneOf
        [ variablePart
        , qualifiedPattern True
        , allPattern
        , unitPattern
        , parensPattern
        , recordPart
        ]


qualifiedPatternArg : Parser State (Node DestructurePattern)
qualifiedPatternArg =
    Combine.oneOf
        [ variablePart
        , qualifiedPattern False
        , allPattern
        , unitPattern
        , parensPattern
        , recordPart
        ]


allPattern : Parser state (Node DestructurePattern)
allPattern =
    Core.succeed AllPattern_
        |. Core.symbol "_"
        |> Node.parserCore
        |> Combine.fromCore


unitPattern : Parser state (Node DestructurePattern)
unitPattern =
    Core.succeed UnitPattern_
        |. Core.symbol "()"
        |> Node.parserCore
        |> Combine.fromCore


qualifiedPattern : ConsumeArgs -> Parser State (Node DestructurePattern)
qualifiedPattern consumeArgs =
    Base.typeIndicator
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                (if consumeArgs then
                    Combine.manyWithEndLocationForLastElement range Node.range (qualifiedPatternArg |> Combine.ignore (Combine.maybeIgnore Layout.layout))

                 else
                    Combine.succeed ( range.end, [] )
                )
                    |> Combine.map
                        (\( end, args ) ->
                            Node
                                { start = range.start, end = end }
                                (NamedPattern_ (QualifiedNameRef mod name) args)
                        )
            )


recordPart : Parser State (Node DestructurePattern)
recordPart =
    Combine.between
        "{"
        "}"
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith (Combine.sepBy "," (Layout.maybeAroundBothSides (Node.parserFromCore Tokens.functionName)))
        )
        |> Combine.map RecordPattern_
        |> Node.parser
