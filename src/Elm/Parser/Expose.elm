module Elm.Parser.Expose exposing (exposable, exposeDefinition, exposingListInner, infixExpose, typeExpose)

import Combine exposing (Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node)


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


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.lazy
        (\() ->
            choice
                [ typeExpose
                , infixExpose
                , functionExpose
                ]
        )


infixExpose : Parser State (Node TopLevelExpose)
infixExpose =
    Combine.lazy
        (\() ->
            Node.parser (Combine.map InfixExpose (parens (while ((/=) ')'))))
        )


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Combine.lazy
        (\() ->
            Node.parser exposedType
        )


exposedType : Parser State TopLevelExpose
exposedType =
    succeed identity
        |> Combine.andMap typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\tipe ->
                Combine.choice
                    [ Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
                        |> Combine.map (Node.range >> Just >> (\v -> ExposedType tipe v) >> TypeExpose)
                    , Combine.succeed (TypeOrAliasExpose tipe)
                    ]
            )


functionExpose : Parser State (Node TopLevelExpose)
functionExpose =
    Node.parser (Combine.map FunctionExpose functionName)
