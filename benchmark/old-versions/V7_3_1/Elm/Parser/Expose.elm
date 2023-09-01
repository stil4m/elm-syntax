module V7_3_1.Elm.Parser.Expose exposing (exposeDefinition, exposingListInner, infixExpose, typeExpose)

import V7_3_1.Combine as Combine exposing (Parser, choice, maybe, or, parens, sepBy, string, succeed, while)
import V7_3_1.Combine.Char exposing (char)
import V7_3_1.Elm.Parser.Layout as Layout
import V7_3_1.Elm.Parser.Node as Node
import V7_3_1.Elm.Parser.Ranges exposing (withRange)
import V7_3_1.Elm.Parser.State exposing (State)
import V7_3_1.Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import V7_3_1.Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import V7_3_1.Elm.Syntax.Node as Node exposing (Node(..))
import V7_3_1.Elm.Syntax.Range as Range


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
        (Combine.map Explicit (sepBy (char ',') (Layout.maybeAroundBothSides exposable)))


exposable : Parser State (Node TopLevelExpose)
exposable =
    choice
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
                Combine.choice
                    [ Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
                        |> Combine.map Node.range
                        |> Combine.map
                            (\openRange ->
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
