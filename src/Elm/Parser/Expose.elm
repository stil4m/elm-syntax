module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    (Tokens.exposingToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterExposing ->
                    \exposingList ->
                        { comments = commentsAfterExposing |> Rope.prependTo exposingList.comments
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
            (Parser.map
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
    Parser.oneOf
        [ Parser.map
            (\( startRow, startColumn ) ->
                \commentsBeforeDotDot ->
                    \commentsAfterDotDot ->
                        \( endRow, endColumn ) ->
                            { comments = commentsBeforeDotDot |> Rope.prependTo commentsAfterDotDot
                            , syntax =
                                All
                                    { start = { row = startRow, column = startColumn }
                                    , end = { row = endRow, column = endColumn }
                                    }
                            }
            )
            Parser.getPosition
            |= Layout.maybeLayout
            |. Tokens.dotDot
            |= Layout.maybeLayout
            |= Parser.getPosition
        , ParserWithComments.sepBy1 "," (Layout.maybeAroundBothSides exposable)
            |> Parser.map
                (\elements ->
                    { comments = elements.comments, syntax = Explicit elements.syntax }
                )
        ]


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    Parser.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]
        |> Node.parser


infixExpose : Parser.Parser (WithComments TopLevelExpose)
infixExpose =
    (Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.map (\infixName -> { comments = Rope.empty, syntax = InfixExpose infixName })
                (Parser.variable
                    { inner = \c -> c /= ')'
                    , reserved = Set.empty
                    , start = \c -> c /= ')'
                    }
                )
            )
    )
        |. Tokens.parensEnd


typeExpose : Parser (WithComments TopLevelExpose)
typeExpose =
    Parser.map
        (\typeName ->
            \open ->
                case open of
                    Nothing ->
                        { comments = Rope.empty, syntax = TypeOrAliasExpose typeName }

                    Just openRange ->
                        { comments = openRange.comments
                        , syntax =
                            TypeExpose { name = typeName, open = Just openRange.syntax }
                        }
        )
        Tokens.typeName
        |= Parser.oneOf
            [ Parser.map
                (\commentsBefore ->
                    \exposingVariantsRangeAndComments ->
                        Just
                            { comments = commentsBefore |> Rope.prependTo exposingVariantsRangeAndComments.comments
                            , syntax = exposingVariantsRangeAndComments.syntax
                            }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= exposingVariants
            , Parser.succeed Nothing
            ]


exposingVariants : Parser (WithComments Range)
exposingVariants =
    Parser.map
        (\( startRow, startColumn ) ->
            \left ->
                \right ->
                    \( endRow, endColumn ) ->
                        { comments = left |> Rope.prependTo right
                        , syntax =
                            { start = { row = startRow, column = startColumn }
                            , end = { row = endRow, column = endColumn }
                            }
                        }
        )
        Parser.getPosition
        |. Tokens.parensStart
        |= Layout.maybeLayout
        |. Parser.symbol ".."
        |= Layout.maybeLayout
        |. Tokens.parensEnd
        |= Parser.getPosition


functionExpose : Parser (WithComments TopLevelExpose)
functionExpose =
    Parser.map (\name -> { comments = Rope.empty, syntax = FunctionExpose name })
        Tokens.functionName
