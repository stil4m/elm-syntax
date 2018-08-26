module Elm.Parser.Expose exposing (exposable, exposeDefinition, exposingListInner, functionExpose, infixExpose, typeExpose)

import Combine exposing (Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged, withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..), ValueConstructorExpose)
import Elm.Syntax.Ranged exposing (Ranged)


exposeDefinition : Parser State Exposing
exposeDefinition =
    exposingToken
        |> Combine.continueWith (maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    parens (Layout.optimisticLayout |> Combine.continueWith exposingListInner |> Combine.ignore Layout.optimisticLayout)


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.lazy
        (\() ->
            or (withRange (succeed All |> Combine.ignore (Layout.maybeAroundBothSides (string ".."))))
                (Combine.map Explicit (sepBy (char ',') (Layout.maybeAroundBothSides exposable)))
        )


exposable : Parser State (Ranged TopLevelExpose)
exposable =
    Combine.lazy
        (\() ->
            choice
                [ typeExpose
                , infixExpose
                , functionExpose
                ]
        )


infixExpose : Parser State (Ranged TopLevelExpose)
infixExpose =
    Combine.lazy
        (\() ->
            ranged (Combine.map InfixExpose (parens (while ((/=) ')'))))
        )


typeExpose : Parser State (Ranged TopLevelExpose)
typeExpose =
    Combine.lazy
        (\() ->
            ranged exposedType
        )


exposedType : Parser State TopLevelExpose
exposedType =
    succeed identity
        |> Combine.andMap typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\tipe ->
                Combine.choice
                    [ ranged (parens (Layout.maybeAroundBothSides (string "..")))
                        |> Combine.map (Tuple.first >> Just >> (\v -> ExposedType tipe v) >> TypeExpose)
                    , Combine.succeed (TypeOrAliasExpose tipe)
                    ]
            )


valueConstructorExpose : Parser State ValueConstructorExpose
valueConstructorExpose =
    ranged typeName


functionExpose : Parser State (Ranged TopLevelExpose)
functionExpose =
    ranged (Combine.map FunctionExpose functionName)
