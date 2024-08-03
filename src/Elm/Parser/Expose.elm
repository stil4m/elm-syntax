module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    Parser.map
        (\() ->
            \commentsAfterExposing ->
                \commentsBefore ->
                    \exposingListInnerResult ->
                        { comments =
                            commentsAfterExposing
                                |> Rope.prependTo commentsBefore
                                |> Rope.prependTo exposingListInnerResult.comments
                        , syntax = exposingListInnerResult.syntax
                        }
        )
        Tokens.exposingToken
        |= Layout.maybeLayoutUntilIgnored Parser.token "("
        |= Layout.optimisticLayout
        |= exposingListInner
        |. Tokens.parensEnd


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    Parser.oneOf
        [ Parser.map
            (\( headStartRow, headStartColumn ) ->
                \headElement ->
                    \( headEndRow, headEndColumn ) ->
                        \commentsAfterHeadElement ->
                            \tailElements ->
                                { comments =
                                    headElement.comments
                                        |> Rope.prependTo commentsAfterHeadElement
                                        |> Rope.prependTo tailElements.comments
                                , syntax =
                                    Explicit
                                        (Node
                                            { start = { row = headStartRow, column = headStartColumn }
                                            , end = { row = headEndRow, column = headEndColumn }
                                            }
                                            headElement.syntax
                                            :: tailElements.syntax
                                        )
                                }
            )
            Parser.getPosition
            |= exposable
            |= Parser.getPosition
            |= Layout.maybeLayout
            |= ParserWithComments.many
                (Tokens.comma
                    |> Parser.Extra.continueWith
                        (Layout.maybeAroundBothSides (exposable |> Node.parser))
                )
        , Parser.map
            (\( startRow, startColumn ) ->
                \commentsAfterDotDot ->
                    \( endRow, endColumn ) ->
                        { comments = commentsAfterDotDot
                        , syntax =
                            All
                                { start = { row = startRow, column = startColumn }
                                , end = { row = endRow, column = endColumn }
                                }
                        }
            )
            Parser.getPosition
            |. Tokens.dotDot
            |= Layout.maybeLayout
            |= Parser.getPosition
        ]


exposable : Parser (WithComments TopLevelExpose)
exposable =
    Parser.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]


infixExpose : Parser.Parser (WithComments TopLevelExpose)
infixExpose =
    (Parser.map (\() -> \infixName -> { comments = Rope.empty, syntax = InfixExpose infixName })
        Tokens.parensStart
        |= Parser.variable
            { inner = \c -> c /= ')'
            , reserved = Set.empty
            , start = \c -> c /= ')'
            }
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
                    \( startRow, startColumn ) ->
                        \left ->
                            \right ->
                                \( endRow, endColumn ) ->
                                    Just
                                        { comments = commentsBefore |> Rope.prependTo left |> Rope.prependTo right
                                        , syntax =
                                            { start = { row = startRow, column = startColumn }
                                            , end = { row = endRow, column = endColumn }
                                            }
                                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= Parser.getPosition
                |. Tokens.parensStart
                |= Layout.maybeLayoutUntilIgnored Parser.token ".."
                |= Layout.maybeLayoutUntilIgnored Parser.token ")"
                |= Parser.getPosition
            , Parser.succeed Nothing
            ]


functionExpose : Parser (WithComments TopLevelExpose)
functionExpose =
    Parser.map (\name -> { comments = Rope.empty, syntax = FunctionExpose name })
        Tokens.functionName
