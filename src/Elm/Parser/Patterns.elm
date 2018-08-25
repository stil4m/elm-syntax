module Elm.Parser.Patterns exposing (ConsumeArgs, composablePattern, listPattern, numberPart, parensPattern, pattern, qualifiedPattern, qualifiedPatternArg, recordPart, tryToCompose, variablePart)

import Combine exposing (Parser, between, choice, lazy, many, maybe, or, parens, sepBy, sepBy1, string, succeed)
import Combine.Num
import Elm.Parser.Base as Base exposing (variablePointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Numbers
import Elm.Parser.Ranges exposing (ranged, rangedWithCustomStart)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (asToken, characterLiteral, functionName, stringLiteral, typeName)
import Elm.Syntax.Base exposing (VariablePointer)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)
import Parser as Core


tryToCompose : Ranged Pattern -> Parser State (Ranged Pattern)
tryToCompose x =
    maybe Layout.layout
        |> Combine.continueWith
            (Combine.choice
                [ Combine.fromCore (Core.keyword "as")
                    |> Combine.ignore Layout.layout
                    |> Combine.continueWith (variablePointer functionName)
                    |> Combine.map
                        (\y ->
                            ( Range.combine [ Tuple.first x, y.range ]
                            , AsPattern x y
                            )
                        )
                , Combine.fromCore (Core.symbol "::")
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith pattern
                    |> Combine.map
                        (\y ->
                            ( Range.combine [ Tuple.first x, Tuple.first y ]
                            , UnConsPattern x y
                            )
                        )
                , Combine.succeed x
                ]
            )


pattern : Parser State (Ranged Pattern)
pattern =
    composablePattern |> Combine.andThen tryToCompose


parensPattern : Parser State (Ranged Pattern)
parensPattern =
    Combine.lazy
        (\() ->
            ranged
                (parens (sepBy (string ",") (Layout.maybeAroundBothSides pattern))
                    |> Combine.map
                        (\c ->
                            case c of
                                [ x ] ->
                                    ParenthesizedPattern x

                                _ ->
                                    TuplePattern c
                        )
                )
        )


variablePart : Parser State (Ranged Pattern)
variablePart =
    ranged (Combine.map VarPattern functionName)


numberPart : Parser State Pattern
numberPart =
    Elm.Parser.Numbers.number FloatPattern IntPattern HexPattern


listPattern : Parser State (Ranged Pattern)
listPattern =
    lazy
        (\() ->
            ranged <|
                between
                    (string "[")
                    (string "]")
                    (Combine.map ListPattern (sepBy (string ",") (Layout.maybeAroundBothSides pattern)))
        )


type alias ConsumeArgs =
    Bool


composablePattern : Parser State (Ranged Pattern)
composablePattern =
    Combine.choice
        [ variablePart
        , qualifiedPattern True
        , ranged (stringLiteral |> Combine.map StringPattern)
        , ranged (characterLiteral |> Combine.map CharPattern)
        , ranged numberPart
        , ranged (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern))
        , ranged (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern))
        , recordPart
        , listPattern
        , parensPattern
        ]


qualifiedPatternArg : Parser State (Ranged Pattern)
qualifiedPatternArg =
    Combine.choice
        [ variablePart
        , qualifiedPattern False
        , ranged (stringLiteral |> Combine.map StringPattern)
        , ranged (characterLiteral |> Combine.map CharPattern)
        , ranged numberPart
        , ranged (Core.symbol "()" |> Combine.fromCore |> Combine.map (always UnitPattern))
        , ranged (Core.symbol "_" |> Combine.fromCore |> Combine.map (always AllPattern))
        , recordPart
        , listPattern
        , parensPattern
        ]


qualifiedPattern : ConsumeArgs -> Parser State ( Range, Pattern )
qualifiedPattern consumeArgs =
    ranged Base.typeIndicator
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\( range, ( mod, name ) ) ->
                (if consumeArgs then
                    many (qualifiedPatternArg |> Combine.ignore (maybe Layout.layout))

                 else
                    Combine.succeed []
                )
                    |> Combine.map
                        (\args ->
                            ( Range.combine (range :: List.map Tuple.first args)
                            , NamedPattern (QualifiedNameRef mod name) args
                            )
                        )
            )


recordPart : Parser State (Ranged Pattern)
recordPart =
    lazy
        (\() ->
            ranged
                (Combine.map RecordPattern <|
                    between
                        (string "{" |> Combine.continueWith (maybe Layout.layout))
                        (maybe Layout.layout |> Combine.continueWith (string "}"))
                        (sepBy1 (string ",") (Layout.maybeAroundBothSides (variablePointer functionName)))
                )
        )
