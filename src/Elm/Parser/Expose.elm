module Elm.Parser.Expose exposing (exposeDefinition)

import CustomParser exposing (Parser)
import CustomParser.Extra
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
    ParserFast.map4
        (\commentsAfterExposing commentsBefore exposingListInnerResult () ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing"
            (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy "(")
        )
        Layout.optimisticLayout
        exposingListInner
        Tokens.parensEnd


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
                (Layout.maybeLayout |> ParserFast.backtrackable)
                (ParserFast.mapWithStartAndEndPosition
                    (\start comments end ->
                        { comments = comments, range = { start = start, end = end } }
                    )
                    (ParserFast.map2 (\left right -> left |> Rope.prependTo right)
                        (ParserFast.symbolFollowedBy "("
                            (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy "..")
                        )
                        (Layout.maybeLayoutUntilIgnored ParserFast.symbolFollowedBy ")")
                    )
                )
            )
            Nothing
        )
        |> Node.parser


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
