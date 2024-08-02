module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope
import Set


exposeDefinition : Parser (WithComments Exposing)
exposeDefinition =
    (Tokens.exposingToken
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsAfterExposing ->
                    \commentsBefore ->
                        \exposingListInnerResult ->
                            { comments =
                                commentsAfterExposing
                                    |> Rope.prependTo commentsBefore
                                    |> Rope.prependTo exposingListInnerResult.comments
                            , syntax = exposingListInnerResult.syntax
                            }
                )
                (Layout.maybeLayoutUntilIgnored Parser.token "(")
            )
    )
        |= Layout.optimisticLayout
        |= exposingListInner
        |. Tokens.parensEnd


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    ParserFast.oneOf2
        (ParserFast.map3
            (\headElement commentsAfterHeadElement tailElements ->
                { comments =
                    headElement.comments
                        |> Rope.prependTo commentsAfterHeadElement
                        |> Rope.prependTo tailElements.comments
                , syntax =
                    Explicit
                        (headElement.syntax
                            :: tailElements.syntax
                        )
                }
            )
            exposable
            Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy ","
                    (Layout.maybeAroundBothSides exposable)
                )
            )
        )
        (ParserFast.mapWithStartAndEndPosition
            (\start commentsAfterDotDot end ->
                { comments = commentsAfterDotDot
                , syntax =
                    All { start = start, end = end }
                }
            )
            (ParserFast.symbolFollowedBy ".." Layout.maybeLayout)
        )


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    ParserFast.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]


infixExpose : ParserFast.Parser (WithComments (Node TopLevelExpose))
infixExpose =
    ParserFast.map2 (\infixName () -> { comments = Rope.empty, syntax = InfixExpose infixName })
        (ParserFast.symbolFollowedBy "("
            (ParserFast.variable
                { inner = \c -> c /= ')'
                , reserved = Set.empty
                , start = \c -> c /= ')'
                }
            )
        )
        Tokens.parensEnd
        |> Node.parser


typeExpose : Parser (WithComments (Node TopLevelExpose))
typeExpose =
    ParserFast.map2
        (\typeName open ->
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
        (ParserFast.orSucceed
            (ParserFast.map2
                (\commentsBefore all ->
                    Just
                        { comments = commentsBefore |> Rope.prependTo all.comments
                        , syntax = all.range
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


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    ParserFast.mapWithStartAndEndPosition
        (\start name end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (FunctionExpose name)
            }
        )
        Tokens.functionName
