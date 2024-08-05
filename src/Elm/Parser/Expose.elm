module Elm.Parser.Expose exposing (exposeDefinition)

import CustomParser exposing (Parser)
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
    CustomParser.map4
        (\commentsAfterExposing commentsBefore exposingListInnerResult () ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = exposingListInnerResult.syntax
            }
        )
        (CustomParser.symbolFollowedBy "exposing"
            (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "(")
        )
        Layout.optimisticLayout
        exposingListInner
        Tokens.parensEnd


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    CustomParser.oneOf2
        (CustomParser.map3
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
                (CustomParser.symbolFollowedBy ","
                    (Layout.maybeAroundBothSides exposable)
                )
            )
        )
        (CustomParser.mapWithStartAndEndPosition
            (\start commentsAfterDotDot end ->
                { comments = commentsAfterDotDot
                , syntax =
                    All { start = start, end = end }
                }
            )
            (CustomParser.symbolFollowedBy ".." Layout.maybeLayout)
        )


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    CustomParser.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]


infixExpose : CustomParser.Parser (WithComments (Node TopLevelExpose))
infixExpose =
    CustomParser.map2 (\infixName () -> { comments = Rope.empty, syntax = InfixExpose infixName })
        (CustomParser.symbolFollowedBy "("
            (CustomParser.variable
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
    CustomParser.map2
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
        (CustomParser.orSucceed
            (CustomParser.map2
                (\commentsBefore all ->
                    Just
                        { comments = commentsBefore |> Rope.prependTo all.comments
                        , syntax = all.range
                        }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                (CustomParser.mapWithStartAndEndPosition
                    (\start comments end ->
                        { comments = comments, range = { start = start, end = end } }
                    )
                    (CustomParser.map2 (\left right -> left |> Rope.prependTo right)
                        (CustomParser.symbolFollowedBy "("
                            (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "..")
                        )
                        (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy ")")
                    )
                )
            )
            Nothing
        )
        |> Node.parser


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    CustomParser.mapWithStartAndEndPosition
        (\start name end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end }
                    (FunctionExpose name)
            }
        )
        Tokens.functionName
