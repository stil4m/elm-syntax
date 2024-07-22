module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))
import Set


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
        [ Combine.succeed
            (\( startRow, startColumn ) ->
                \( endRow, endColumn ) ->
                    All
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
            )
            |> Combine.keepFromCore Core.getPosition
            |> Combine.ignore (Layout.maybeAroundBothSides (Combine.fromCore Tokens.dotDot))
            |> Combine.keepFromCore Core.getPosition
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
        |= Core.variable
            { start = \c -> c /= ')'
            , inner = \c -> c /= ')'
            , reserved = Set.empty
            }
        |. Tokens.parensEnd
        |> Node.parserFromCore


typeExpose : Parser State (Node TopLevelExpose)
typeExpose =
    Node.parserCore Tokens.typeName
        |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.andThen
            (\((Node typeRange typeValue) as tipe) ->
                Combine.oneOf
                    [ exposingVariants
                        |> Combine.map
                            (\(Node openRange _) ->
                                Node
                                    { start = typeRange.start, end = openRange.end }
                                    (TypeExpose (ExposedType typeValue (Just openRange)))
                            )
                    , Combine.succeedLazy (\() -> Node.map TypeOrAliasExpose tipe)
                    ]
            )


exposingVariants : Parser State (Node ())
exposingVariants =
    Node.parser (Combine.parens (Layout.maybeAroundBothSides (Combine.symbol "..")))


functionExpose : Parser state (Node TopLevelExpose)
functionExpose =
    Node.parserFromCore (Core.map FunctionExpose Tokens.functionName)
