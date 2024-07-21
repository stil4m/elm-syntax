module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))
import Parser.Extra


exposeDefinition : Parser State Exposing
exposeDefinition =
    Tokens.exposingToken
        |> Combine.continueWithFromCore (Combine.maybeIgnore Layout.layout)
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
            |> Combine.keepFromCore Parser.Extra.location
            |> Combine.ignore (Layout.maybeAroundBothSides (Combine.fromCore Tokens.dotDot))
            |> Combine.keepFromCore Parser.Extra.location
        , Combine.sepBy1 "," (Layout.maybeAroundBothSides exposable)
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
        |. Tokens.parensStart
        |= (Core.chompWhile (\c -> c /= ')')
                |> Core.getChompedString
           )
        |. Tokens.parensEnd
        |> Node.parserFromCore


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Node.parserCore Tokens.typeName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
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
                    , Combine.lazy (\() -> Combine.succeed (Node.map TypeOrAliasExpose tipe))
                    ]
            )


functionExpose : Parser state (Node TopLevelExpose)
functionExpose =
    Node.parserFromCore (Core.map FunctionExpose Tokens.functionName)
