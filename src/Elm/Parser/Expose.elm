module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))
import Set


exposeDefinition : Parser State Exposing
exposeDefinition =
    Tokens.exposingToken
        |> Combine.continueWithFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith exposeListWith


exposeListWith : Parser State Exposing
exposeListWith =
    Combine.between
        Tokens.parensStart
        Tokens.parensEnd
        (Layout.optimisticLayout
            |> Combine.continueWith exposingListInner
            |> Combine.ignore Layout.optimisticLayout
        )


exposingListInner : Parser State Exposing
exposingListInner =
    Combine.oneOf
        [ Core.map
            (\( startRow, startColumn ) ->
                \( endRow, endColumn ) ->
                    All
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
            )
            Core.getPosition
            |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
            |> Combine.ignoreEntirely Tokens.dotDot
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            |> Combine.keepFromCore Core.getPosition
        , Combine.sepBy1 "," (Layout.maybeAroundBothSides exposable)
            |> Combine.map Explicit
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.oneOf
        [ typeExpose
        , infixExpose |> Combine.fromCore
        , functionExpose |> Combine.fromCore
        ]
        |> Node.parser


infixExpose : Core.Parser TopLevelExpose
infixExpose =
    Core.map (\() -> InfixExpose)
        Tokens.parensStart
        |= Core.variable
            { inner = \c -> c /= ')'
            , reserved = Set.empty
            , start = \c -> c /= ')'
            }
        |. Tokens.parensEnd


typeExpose : Parser State TopLevelExpose
typeExpose =
    Tokens.typeName
        |> Combine.andThenFromCore
            (\typeValue ->
                Combine.oneOf
                    [ Combine.map
                        (\() ->
                            \(Node openRange ()) ->
                                TypeExpose (ExposedType typeValue (Just openRange))
                        )
                        (Combine.maybeIgnore Layout.layout |> Combine.backtrackable)
                        |> Combine.keep exposingVariants
                    , Combine.succeedLazy (\() -> TypeOrAliasExpose typeValue)
                    ]
            )


exposingVariants : Parser State (Node ())
exposingVariants =
    Node.parser
        (Combine.between
            Tokens.parensStart
            Tokens.parensEnd
            (Combine.maybeIgnore Layout.layout
                |> Combine.ignoreEntirely (Core.symbol "..")
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            )
        )


functionExpose : Core.Parser TopLevelExpose
functionExpose =
    Core.map FunctionExpose Tokens.functionName
