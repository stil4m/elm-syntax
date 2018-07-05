module Elm.Parser.Patterns exposing (pattern)

import Combine exposing (Parser, between, lazy, many, maybe, parens, sepBy, sepBy1, string)
import Combine.Extra as Combine
import Elm.Parser.Base as Base exposing (variablePointer)
import Elm.Parser.Layout as Layout
import Elm.Parser.Numbers
import Elm.Parser.Ranges exposing (ranged)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (characterLiteral, functionName, stringLiteral)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Ranged exposing (Ranged)


tryToCompose : Ranged Pattern -> Parser State (Ranged Pattern)
tryToCompose x =
    lazy
        (\() ->
            maybe Layout.layout
                |> Combine.continueWith
                    (Combine.choice
                        [ Combine.string "as"
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith (variablePointer functionName)
                            |> Combine.map
                                (\y ->
                                    ( Range.combine [ Tuple.first x, y.range ]
                                    , AsPattern x y
                                    )
                                )
                        , Combine.string "::"
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
        )


pattern : Parser State (Ranged Pattern)
pattern =
    lazy
        (\() ->
            composablePattern |> Combine.andThen tryToCompose
        )


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
    lazy
        (\() ->
            Combine.choice
                [ variablePart
                , qualifiedPattern True
                , ranged (stringLiteral |> Combine.map StringPattern)
                , ranged (characterLiteral |> Combine.map CharPattern)
                , ranged numberPart
                , ranged (Combine.string "()" |> Combine.map (always UnitPattern))
                , ranged (Combine.string "_" |> Combine.map (always AllPattern))
                , recordPart
                , listPattern
                , parensPattern
                ]
        )


qualifiedPatternArg : Parser State (Ranged Pattern)
qualifiedPatternArg =
    lazy
        (\() ->
            Combine.choice
                [ variablePart
                , qualifiedPattern False
                , ranged (stringLiteral |> Combine.map StringPattern)
                , ranged (characterLiteral |> Combine.map CharPattern)
                , ranged numberPart
                , ranged (Combine.string "()" |> Combine.map (always UnitPattern))
                , ranged (Combine.string "_" |> Combine.map (always AllPattern))
                , recordPart
                , listPattern
                , parensPattern
                ]
        )


qualifiedPattern : ConsumeArgs -> Parser State ( Range, Pattern )
qualifiedPattern consumeArgs =
    lazy
        (\() ->
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
