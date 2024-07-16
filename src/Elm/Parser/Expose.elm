module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionNameCore, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))


exposeDefinition : Parser State Exposing
exposeDefinition =
    exposingToken
        |> Combine.continueWith (Combine.maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    Combine.parens
        (Layout.optimisticLayout
            |> Combine.continueWith exposingListInner
            |> Combine.ignore Layout.optimisticLayout
        )


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.oneOf
        [ Combine.succeed (\start -> \end -> All { start = start, end = end })
            |> Combine.keep Combine.location
            |> Combine.ignore (Layout.maybeAroundBothSides (Combine.symbol ".."))
            |> Combine.keep Combine.location
        , Combine.sepBy1 (Combine.symbol ",") (Layout.maybeAroundBothSides exposable)
            |> Combine.map Explicit
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.oneOf
        [ typeExpose
        , infixExpose
        , functionExpose
        ]


infixExpose : Parser state (Node TopLevelExpose)
infixExpose =
    Core.succeed InfixExpose
        |. Core.symbol "("
        |= (Core.chompWhile (\c -> c /= ')')
                |> Core.getChompedString
           )
        |. Core.symbol ")"
        |> Node.parserFromCore


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Node.parserFromCore typeName
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\((Node typeRange typeValue) as tipe) ->
                Combine.oneOf
                    [ Node.parser (Combine.parens (Layout.maybeAroundBothSides (Combine.symbol "..")))
                        |> Combine.map
                            (\(Node openRange _) ->
                                Node
                                    { start = typeRange.start, end = openRange.end }
                                    (TypeExpose (ExposedType typeValue (Just openRange)))
                            )
                    , Combine.succeed ()
                        |> Combine.map (\() -> Node.map TypeOrAliasExpose tipe)
                    ]
            )


functionExpose : Parser state (Node TopLevelExpose)
functionExpose =
    Node.parserFromCore (Core.map FunctionExpose functionNameCore)
