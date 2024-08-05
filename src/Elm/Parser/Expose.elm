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
    CustomParser.oneOf
        [ CustomParser.map5
            (\headStart headElement headEnd commentsAfterHeadElement tailElements ->
                { comments =
                    headElement.comments
                        |> Rope.prependTo commentsAfterHeadElement
                        |> Rope.prependTo tailElements.comments
                , syntax =
                    Explicit
                        (Node
                            { start = headStart
                            , end = headEnd
                            }
                            headElement.syntax
                            :: tailElements.syntax
                        )
                }
            )
            CustomParser.getPosition
            exposable
            CustomParser.getPosition
            Layout.maybeLayout
            (ParserWithComments.many
                (CustomParser.symbolFollowedBy ","
                    (Layout.maybeAroundBothSides (exposable |> Node.parser))
                )
            )
        , CustomParser.map3
            (\start commentsAfterDotDot end ->
                { comments = commentsAfterDotDot
                , syntax =
                    All { start = start, end = end }
                }
            )
            CustomParser.getPosition
            (CustomParser.symbolFollowedBy ".." Layout.maybeLayout)
            CustomParser.getPosition
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
        (CustomParser.oneOf
            [ CustomParser.map5
                (\commentsBefore start left right end ->
                    Just
                        { comments = commentsBefore |> Rope.prependTo left |> Rope.prependTo right
                        , syntax = { start = start, end = end }
                        }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                CustomParser.getPosition
                (CustomParser.symbolFollowedBy "("
                    (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "..")
                )
                (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy ")")
                CustomParser.getPosition
            , CustomParser.succeed Nothing
            ]
        )


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    CustomParser.map (\name -> { comments = Rope.empty, syntax = FunctionExpose name })
        Tokens.functionName
