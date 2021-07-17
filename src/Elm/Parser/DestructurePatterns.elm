module Elm.Parser.DestructurePatterns exposing (destructurPattern)

import Combine exposing (Parser, between, lazy, many, maybe, parens, sepBy, string)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range as Range
import Parser as Core


tryToCompose : Node DestructurePattern -> Parser State (Node DestructurePattern)
tryToCompose x =
    maybe Layout.layout
        |> Combine.continueWith
            (Combine.choice
                [ Combine.fromCore (Core.keyword "as")
                    |> Combine.ignore Layout.layout
                    |> Combine.continueWith (Node.parser functionName)
                    |> Combine.map (\y -> Node.combine AsPattern_ x y)
                , Combine.succeed x
                ]
            )


destructurPattern : Parser State (Node DestructurePattern)
destructurPattern =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State (Node DestructurePattern)
parensPattern =
    Combine.lazy
        (\() ->
            Node.parser
                (parens (sepBy (string ",") (Layout.maybeAroundBothSides destructurPattern))
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
    Node.parser (Combine.map VarPattern_ functionName)


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Node DestructurePattern)
composablePattern =
    Combine.choice
        [ variablePart
        , qualifiedPattern True
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern_))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern_))
        , recordPart
        , parensPattern
        ]


qualifiedPatternArg : Parser State (Node DestructurePattern)
qualifiedPatternArg =
    Combine.choice
        [ variablePart
        , qualifiedPattern False
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern_))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern_))
        , recordPart
        , parensPattern
        ]


qualifiedPattern : ConsumeArgs -> Parser State (Node DestructurePattern)
qualifiedPattern consumeArgs =
    Node.parser Base.typeIndicator
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\(Node range ( mod, name )) ->
                (if consumeArgs then
                    many (qualifiedPatternArg |> Combine.ignore (maybe Layout.layout))

                 else
                    Combine.succeed []
                )
                    |> Combine.map
                        (\args ->
                            Node
                                (Range.combine (range :: List.map (\(Node r _) -> r) args))
                                (NamedPattern_ (QualifiedNameRef mod name) args)
                        )
            )


recordPart : Parser State (Node DestructurePattern)
recordPart =
    lazy
        (\() ->
            Node.parser
                (Combine.map RecordPattern_ <|
                    between
                        (string "{" |> Combine.continueWith (maybe Layout.layout))
                        (maybe Layout.layout |> Combine.continueWith (string "}"))
                        (sepBy (string ",") (Layout.maybeAroundBothSides (Node.parser functionName)))
                )
        )
