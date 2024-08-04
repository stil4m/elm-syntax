module Elm.Parser.Expose exposing (exposeDefinition)

import CustomParser exposing (Parser)
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
    CustomParser.map5
        (\() commentsAfterExposing commentsBefore exposingListInnerResult () ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = exposingListInnerResult.syntax
            }
        )
        Tokens.exposingToken
        (Layout.maybeLayoutUntilIgnored CustomParser.token "(")
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
                (Tokens.comma
                    |> CustomParser.Extra.continueWith
                        (Layout.maybeAroundBothSides (exposable |> Node.parser))
                )
            )
        , CustomParser.map4
            (\start () commentsAfterDotDot end ->
                { comments = commentsAfterDotDot
                , syntax =
                    All { start = start, end = end }
                }
            )
            CustomParser.getPosition
            Tokens.dotDot
            Layout.maybeLayout
            CustomParser.getPosition
        ]


exposable : Parser (WithComments TopLevelExpose)
exposable =
    CustomParser.oneOf
        [ functionExpose
        , typeExpose
        , infixExpose
        ]


infixExpose : CustomParser.Parser (WithComments TopLevelExpose)
infixExpose =
    CustomParser.map3 (\() infixName () -> { comments = Rope.empty, syntax = InfixExpose infixName })
        Tokens.parensStart
        (CustomParser.variable
            { inner = \c -> c /= ')'
            , reserved = Set.empty
            , start = \c -> c /= ')'
            }
        )
        Tokens.parensEnd


typeExpose : Parser (WithComments TopLevelExpose)
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
            [ CustomParser.map6
                (\commentsBefore start () left right end ->
                    Just
                        { comments = commentsBefore |> Rope.prependTo left |> Rope.prependTo right
                        , syntax = { start = start, end = end }
                        }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                CustomParser.getPosition
                Tokens.parensStart
                (Layout.maybeLayoutUntilIgnored CustomParser.token "..")
                (Layout.maybeLayoutUntilIgnored CustomParser.token ")")
                CustomParser.getPosition
            , CustomParser.succeed Nothing
            ]
        )


functionExpose : Parser (WithComments TopLevelExpose)
functionExpose =
    CustomParser.map (\name -> { comments = Rope.empty, syntax = FunctionExpose name })
        Tokens.functionName
