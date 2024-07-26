module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Parser as Core exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    (Tokens.exposingToken
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsAfterExposing ->
                    \exposingList ->
                        { comments = Rope.flatFromList [ commentsAfterExposing, exposingList.comments ]
                        , syntax = exposingList.syntax
                        }
                )
                Layout.maybeLayout
            )
    )
        |= exposeListWith


exposeListWith : Parser (WithComments Exposing)
exposeListWith =
    (Tokens.parensStart
        |> Parser.Extra.continueWith
            (Core.map
                (\commentsBefore ->
                    \exposingListInnerResult ->
                        \commentsAfter ->
                            { comments = Rope.flatFromList [ commentsBefore, exposingListInnerResult.comments, commentsAfter ]
                            , syntax = exposingListInnerResult.syntax
                            }
                )
                Layout.optimisticLayout
            )
    )
        |= exposingListInner
        |= Layout.optimisticLayout
        |. Tokens.parensEnd


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    Core.oneOf
        [ Core.map
            (\( startRow, startColumn ) ->
                \commentsBeforeDotDot ->
                    \commentsAfterDotDot ->
                        \( endRow, endColumn ) ->
                            { comments = Rope.flatFromList [ commentsBeforeDotDot, commentsAfterDotDot ]
                            , syntax =
                                All
                                    { start = { row = startRow, column = startColumn }
                                    , end = { row = endRow, column = endColumn }
                                    }
                            }
            )
            Core.getPosition
            |= Layout.maybeLayout
            |. Tokens.dotDot
            |= Layout.maybeLayout
            |= Core.getPosition
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

                    Just openRange ->
                        TypeExpose { name = typeName, open = Just openRange }
        )
        Tokens.typeName
        |> ParserWithComments.fromCoreKeep
            (ParserWithComments.maybe
                (Core.map
                    (\commentsBefore ->
                        \exposingVariantsRangeAndComments ->
                            { comments = Rope.flatFromList [ commentsBefore, exposingVariantsRangeAndComments.comments ]
                            , syntax = exposingVariantsRangeAndComments.syntax
                            }
                    )
                    (Layout.maybeLayout |> Core.backtrackable)
                    |= exposingVariants
                )
            )


exposingVariants : Parser (WithComments Range)
exposingVariants =
    Core.map
        (\( startRow, startColumn ) ->
            \left ->
                \right ->
                    \( endRow, endColumn ) ->
                        { comments = Rope.flatFromList [ left, right ]
                        , syntax =
                            { start = { row = startRow, column = startColumn }
                            , end = { row = endRow, column = endColumn }
                            }
                        }
        )
        Core.getPosition
        |. Tokens.parensStart
        |= Layout.maybeLayout
        |. Core.symbol ".."
        |= Layout.maybeLayout
        |. Tokens.parensEnd
        |= Core.getPosition


functionExpose : Parser (WithComments TopLevelExpose)
functionExpose =
    ParserWithComments.fromCoreMap FunctionExpose Tokens.functionName
