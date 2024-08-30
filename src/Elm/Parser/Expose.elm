module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


exposeDefinition : Parser (WithComments (Node Exposing))
exposeDefinition =
    ParserFast.map3WithRange
        (\range commentsAfterExposing commentsBefore exposingListInnerResult ->
            { comments =
                commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo exposingListInnerResult.comments
            , syntax = Node range exposingListInnerResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "exposing"
            (Layout.maybeLayout |> ParserFast.followedBySymbol "(")
        )
        Layout.optimisticLayout
        (exposingListInner
            |> ParserFast.followedBySymbol ")"
        )


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
        (ParserFast.mapWithRange
            (\range commentsAfterDotDot ->
                { comments = commentsAfterDotDot
                , syntax = All range
                }
            )
            (ParserFast.symbolFollowedBy ".." Layout.maybeLayout)
        )


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : ParserFast.Parser (WithComments (Node TopLevelExpose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            { comments = Rope.empty
            , syntax = Node range (InfixExpose infixName)
            }
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhileWithoutLinebreak
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
            )
        )
        Tokens.parensEnd


typeExpose : Parser (WithComments (Node TopLevelExpose))
typeExpose =
    ParserFast.map2WithRange
        (\range typeName open ->
            case open of
                Nothing ->
                    { comments = Rope.empty
                    , syntax = Node range (TypeOrAliasExpose typeName)
                    }

                Just openRange ->
                    { comments = openRange.comments
                    , syntax = Node range (TypeExpose { name = typeName, open = Just openRange.syntax })
                    }
        )
        Tokens.typeName
        (ParserFast.map2OrSucceed
            (\commentsBefore all ->
                Just
                    { comments = commentsBefore |> Rope.prependTo all.comments
                    , syntax = all.range
                    }
            )
            Layout.maybeLayoutBacktrackable
            (ParserFast.map2WithRange
                (\range left right ->
                    { comments = left |> Rope.prependTo right, range = range }
                )
                (ParserFast.symbolFollowedBy "("
                    (Layout.maybeLayout |> ParserFast.followedBySymbol "..")
                )
                (Layout.maybeLayout |> ParserFast.followedBySymbol ")")
            )
            Nothing
        )


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    Tokens.functionNameMapWithRange
        (\range name ->
            { comments = Rope.empty
            , syntax =
                Node range (FunctionExpose name)
            }
        )
