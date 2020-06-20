module Elm.Parser.Expose exposing (exposeDefinition)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=))
import Set


exposeDefinition : Parser State Exposing
exposeDefinition =
    Tokens.exposingToken
        |> Combine.continueWithFromCore Layout.maybeLayout
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
            |> Combine.fromCoreIgnore Layout.maybeLayout
            |> Combine.ignoreEntirely Tokens.dotDot
            |> Combine.ignore Layout.maybeLayout
            |> Combine.keepFromCore Core.getPosition
        , Combine.succeed (\head -> \tail -> Explicit head tail)
            |> Combine.keep (Layout.maybeAroundBothSides exposable)
            |> Combine.keep (Combine.many (Tokens.comma |> Combine.continueWithFromCore (Layout.maybeAroundBothSides exposable)))
        ]


exposable : Parser State (Node TopLevelExpose)
exposable =
    Combine.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose |> Combine.fromCore
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
    Core.map
        (\typeName ->
            \open ->
                case open of
                    Nothing ->
                        TypeOrAliasExpose typeName

                    Just (Node openRange ()) ->
                        TypeExpose { name = typeName, open = Just openRange }
        )
        Tokens.typeName
        |> Combine.fromCoreKeep
            (Combine.maybe
                ((Layout.maybeLayout |> Combine.backtrackable)
                    |> Combine.continueWith exposingVariants
                )
            )


exposingVariants : Parser State (Node ())
exposingVariants =
    Node.parser
        (Combine.between
            Tokens.parensStart
            Tokens.parensEnd
            (Layout.maybeLayout
                |> Combine.ignoreEntirely (Core.symbol "..")
                |> Combine.ignore Layout.maybeLayout
            )
        )


functionExpose : Parser state TopLevelExpose
functionExpose =
    Combine.fromCoreMap FunctionExpose Tokens.functionName
