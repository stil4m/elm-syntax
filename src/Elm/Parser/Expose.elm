module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    Tokens.exposingToken
        |> Parser.Extra.continueWith Layout.maybeLayout
        |> ParserWithComments.continueWith exposeListWith


exposeListWith : Parser (WithComments Exposing)
exposeListWith =
    ParserWithComments.between
        Tokens.parensStart
        Tokens.parensEnd
        (Layout.optimisticLayout
            |> ParserWithComments.continueWith exposingListInner
            |> ParserWithComments.ignore Layout.optimisticLayout
        )


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    Core.oneOf
        [ (Core.map
            (\( startRow, startColumn ) ->
                \( endRow, endColumn ) ->
                    All
                        { start = { row = startRow, column = startColumn }
                        , end = { row = endRow, column = endColumn }
                        }
            )
            Core.getPosition
            |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
          )
            |. Tokens.dotDot
            |> ParserWithComments.ignore Layout.maybeLayout
            |> ParserWithComments.keepFromCore Core.getPosition
        , ParserWithComments.sepBy1 "," (Layout.maybeAroundBothSides exposable)
            |> ParserWithComments.map Explicit
        ]


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    Core.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose |> ParserWithComments.fromCore
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


typeExpose : Parser (WithComments TopLevelExpose)
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
        |> ParserWithComments.fromCoreKeep
            (ParserWithComments.maybe
                ((Layout.maybeLayout |> Core.backtrackable)
                    |> ParserWithComments.continueWith exposingVariants
                )
            )


exposingVariants : Parser (WithComments (Node ()))
exposingVariants =
    Node.parser
        (ParserWithComments.between
            Tokens.parensStart
            Tokens.parensEnd
            (Layout.maybeLayout
                |. Core.symbol ".."
                |> ParserWithComments.ignore Layout.maybeLayout
            )
        )


functionExpose : Parser (WithComments TopLevelExpose)
functionExpose =
    ParserWithComments.fromCoreMap FunctionExpose Tokens.functionName
