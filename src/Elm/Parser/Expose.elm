module Elm.Parser.Expose exposing (exposeDefinition)

import CustomParser exposing (Parser )
import CustomParser.Extra
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import ParserWithComments exposing (WithComments)
import Rope
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    CustomParser.map
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
        |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "(")
        |> CustomParser.keep Layout.optimisticLayout
        |> CustomParser.keep exposingListInner
        |> CustomParser.ignore Tokens.parensEnd


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    CustomParser.oneOf
        [ CustomParser.map
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
            CustomParser.getPosition
            |> CustomParser.keep exposable
            |> CustomParser.keep CustomParser.getPosition
            |> CustomParser.keep Layout.maybeLayout
            |> CustomParser.keep
                (ParserWithComments.many
                    (Tokens.comma
                        |> CustomParser.Extra.continueWith
                            (Layout.maybeAroundBothSides (exposable |> Node.parser))
                    )
                )
        , CustomParser.map
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
            CustomParser.getPosition
            |> CustomParser.ignore Tokens.dotDot
            |> CustomParser.keep Layout.maybeLayout
            |> CustomParser.keep CustomParser.getPosition
        ]


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    CustomParser.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]


infixExpose : CustomParser.Parser (WithComments TopLevelExpose)
infixExpose =
    (CustomParser.map (\() -> \infixName -> { comments = Rope.empty, syntax = InfixExpose infixName })
        Tokens.parensStart
        |> CustomParser.keep
            (CustomParser.variable
                { inner = \c -> c /= ')'
                , reserved = Set.empty
                , start = \c -> c /= ')'
                }
            )
    )
        |> CustomParser.ignore Tokens.parensEnd


typeExpose : Parser (WithComments (Node TopLevelExpose))
typeExpose =
    CustomParser.map
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
        |> CustomParser.keep
            (CustomParser.oneOf
                [ CustomParser.map
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
                    (Layout.maybeLayout |> CustomParser.backtrackable)
                    |> CustomParser.keep CustomParser.getPosition
                    |> CustomParser.ignore Tokens.parensStart
                    |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token "..")
                    |> CustomParser.keep (Layout.maybeLayoutUntilIgnored CustomParser.token ")")
                    |> CustomParser.keep CustomParser.getPosition
                , CustomParser.succeed Nothing
                ]
            )


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    CustomParser.map (\name -> { comments = Rope.empty, syntax = FunctionExpose name })
        Tokens.functionName
