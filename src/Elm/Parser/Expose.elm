module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser, maybe, oneOf, or, parens, sepBy1, string, succeed, while)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Ranges exposing (withRange)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range


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
    or (withRange (succeed All |> Combine.ignore (Layout.maybeAroundBothSides (string ".."))))
        (Combine.map Explicit (sepBy1 (char ',') (Layout.maybeAroundBothSides exposable)))


exposable : Parser State (Node TopLevelExpose)
exposable =
    oneOf
        [ typeExpose
        , infixExpose
        , functionExpose
        ]


infixExpose : Parser State (Node TopLevelExpose)
infixExpose =
    Node.parser (Combine.map InfixExpose (parens (while ((/=) ')'))))


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Node.parser typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\tipe ->
                Combine.oneOf
                    [ Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
                        |> Combine.map
                            (\(Node openRange _) ->
                                Node
                                    (Range.combine [ Node.range tipe, openRange ])
                                    (TypeExpose (ExposedType (Node.value tipe) (Just openRange)))
                            )
                    , Combine.succeed (Node.map TypeOrAliasExpose tipe)
                    ]
            )


functionExpose : Parser State (Node TopLevelExpose)
functionExpose =
    Node.parser (Combine.map FunctionExpose functionName)
