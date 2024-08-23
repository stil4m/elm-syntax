module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


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
            (Layout.maybeLayoutUntilIgnored ParserFast.symbol "(")
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
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : ParserFast.Parser (WithComments (Node TopLevelExpose))
infixExpose =
    ParserFast.map2WithStartAndEndPosition
        (\start infixName () end ->
            { comments = Rope.empty
            , syntax = Node { start = start, end = end } (InfixExpose infixName)
            }
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhile
                (\c -> c /= ')')
                (\c -> c /= ')')
            )
        )
        Tokens.parensEnd


typeExpose : Parser (WithComments (Node TopLevelExpose))
typeExpose =
    ParserFast.map2WithStartAndEndPosition
        (\start typeName open end ->
            case open of
                Nothing ->
                    { comments = Rope.empty
                    , syntax =
                        Node { start = start, end = end }
                            (TypeOrAliasExpose typeName)
                    }

                Just openRange ->
                    { comments = openRange.comments
                    , syntax =
                        Node { start = start, end = end }
                            (TypeExpose { name = typeName, open = Just openRange.syntax })
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
                Layout.maybeLayoutBacktrackable
                (ParserFast.map2WithStartAndEndPosition
                    (\start left right end ->
                        { comments = left |> Rope.prependTo right, range = { start = start, end = end } }
                    )
                    (ParserFast.symbolFollowedBy "("
                        (Layout.maybeLayoutUntilIgnored ParserFast.symbol "..")
                    )
                    (Layout.maybeLayoutUntilIgnored ParserFast.symbol ")")
                )
            )
            Nothing
        )


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
