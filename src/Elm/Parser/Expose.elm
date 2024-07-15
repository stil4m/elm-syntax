module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser, maybe, oneOf, parens, sepBy1, string, symbol)
import Combine.Char exposing (char)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (exposingToken, functionName, typeName)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))


exposeDefinition : Parser State Exposing
exposeDefinition =
    exposingToken
        |> Combine.continueWith (maybe Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    parens
        (Layout.optimisticLayout
            |> Combine.continueWith exposingListInner
            |> Combine.ignore Layout.optimisticLayout
        )


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.oneOf
        [ Combine.succeed (\start -> \end -> All { start = start, end = end })
            |> Combine.keep Combine.location
            |> Combine.ignore (Layout.maybeAroundBothSides (symbol ".."))
            |> Combine.keep Combine.location
        , sepBy1 (char ',') (Layout.maybeAroundBothSides exposable)
            |> Combine.map Explicit
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    oneOf
        [ typeExpose
        , infixExpose
        , functionExpose
        ]


infixExpose : Parser state (Node TopLevelExpose)
infixExpose =
    Core.succeed identity
        |. Core.symbol "("
        |= (Core.chompWhile (\c -> c /= ')')
                |> Core.getChompedString
           )
        |. Core.symbol ")"
        |> Core.map InfixExpose
        |> Node.parserCore
        |> Combine.fromCore


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Node.parser typeName
        |> Combine.ignore (maybe Layout.layout)
        |> Combine.andThen
            (\((Node typeRange typeValue) as tipe) ->
                Combine.oneOf
                    [ Node.parser (parens (Layout.maybeAroundBothSides (string "..")))
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
    Node.parser (Combine.map FunctionExpose functionName)
