module Elm.Parser.DeconstructPatterns exposing (deconstructPattern)

import Combine exposing (Parser, between, choice, lazy, many, maybe, parens, sepBy, string, succeed)
import Elm.Parser.Base as Base
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Numbers
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (characterLiteral, functionName, stringLiteral)
import Elm.Syntax.DeconstructPattern exposing (DeconstructPattern(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range
import Parser as Core


tryToCompose : Node DeconstructPattern -> Parser State (Node DeconstructPattern)
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


deconstructPattern : Parser State (Node DeconstructPattern)
deconstructPattern =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State (Node DeconstructPattern)
parensPattern =
    Combine.lazy
        (\() ->
            Node.parser
                (parens (sepBy (string ",") (Layout.maybeAroundBothSides deconstructPattern))
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


variablePart : Parser State (Node DeconstructPattern)
variablePart =
    Node.parser (Combine.map VarPattern_ functionName)


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Node DeconstructPattern)
composablePattern =
    Combine.choice
        [ variablePart
        , qualifiedPattern True
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern_))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern_))
        , recordPart
        , parensPattern
        ]


qualifiedPatternArg : Parser State (Node DeconstructPattern)
qualifiedPatternArg =
    Combine.choice
        [ variablePart
        , qualifiedPattern False
        , Node.parser (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern_))
        , Node.parser (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern_))
        , recordPart
        , parensPattern
        ]


qualifiedPattern : ConsumeArgs -> Parser State (Node DeconstructPattern)
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


recordPart : Parser State (Node DeconstructPattern)
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
