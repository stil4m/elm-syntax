module Elm.Parser.DestructurePatterns exposing (patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)


destructurePattern : Parser (WithComments (Node DestructurePattern))
destructurePattern =
    ParserFast.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node DestructurePattern))
composablePatternTryToCompose =
    ParserFast.map3
        (\x commentsAfterLeft maybeComposedWithResult ->
            { comments =
                x.comments
                    |> Rope.prependTo commentsAfterLeft
                    |> Rope.prependTo maybeComposedWithResult.comments
            , syntax =
                case maybeComposedWithResult.syntax of
                    PatternComposedWithNothing () ->
                        x.syntax

                    PatternComposedWithAs anotherName ->
                        Node.combine AsPattern_ x.syntax anotherName
            }
        )
        composablePattern
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    ParserFast.orSucceed
        (ParserFast.keywordFollowedBy "as"
            (ParserFast.map2
                (\commentsAfterAs name ->
                    { comments = commentsAfterAs
                    , syntax = PatternComposedWithAs name
                    }
                )
                Layout.maybeLayout
                Tokens.functionNameNode
            )
        )
        { comments = Rope.empty, syntax = PatternComposedWithNothing () }


parensPattern : Parser (WithComments (Node DestructurePattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> Rope.prependTo contentResult.comments
                , syntax =
                    Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResult.syntax
                }
            )
            Layout.maybeLayout
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = Rope.empty, syntax = UnitPattern_ })
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        { comments =
                            headResult.comments
                                |> Rope.prependTo commentsAfterHead
                                |> Rope.prependTo tailResult.comments
                        , syntax =
                            case tailResult.syntax of
                                Nothing ->
                                    ParenthesizedPattern_ headResult.syntax

                                Just secondAndMaybeThirdPart ->
                                    case secondAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            TuplePattern_ [ headResult.syntax, secondAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            TuplePattern_ [ headResult.syntax, secondAndMaybeThirdPart.secondPart, thirdPart ]
                        }
                    )
                    destructurePattern
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo secondPart.comments
                                            |> Rope.prependTo commentsAfter
                                            |> Rope.prependTo maybeThirdPart.comments
                                    , syntax = Just { maybeThirdPart = maybeThirdPart.syntax, secondPart = secondPart.syntax }
                                    }
                                )
                                Layout.maybeLayout
                                destructurePattern
                                Layout.maybeLayout
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> Rope.prependTo thirdPart.comments
                                                        |> Rope.prependTo commentsAfter
                                                , syntax = Just thirdPart.syntax
                                                }
                                            )
                                            Layout.maybeLayout
                                            destructurePattern
                                            Layout.maybeLayout
                                            |> ParserFast.followedBySymbol ")"
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


varPattern : Parser (WithComments (Node DestructurePattern))
varPattern =
    Tokens.functionNameMapWithRange
        (\range var ->
            { comments = Rope.empty
            , syntax = Node range (VarPattern_ var)
            }
        )


composablePattern : Parser (WithComments (Node DestructurePattern))
composablePattern =
    ParserFast.oneOf5
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        parensPattern
        recordPattern


patternNotDirectlyComposing : Parser (WithComments (Node DestructurePattern))
patternNotDirectlyComposing =
    ParserFast.oneOf5
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        parensPattern
        recordPattern


allPattern : Parser (WithComments (Node DestructurePattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            { comments = Rope.empty
            , syntax = Node range AllPattern_
            }
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.orSucceed
        (ParserFast.map2
            (\startName afterStartName ->
                case afterStartName of
                    Nothing ->
                        Just ( [], startName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( startName :: qualificationAfter, unqualified )
            )
            (ParserFast.symbolFollowedBy "." Tokens.typeName)
            (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        )
        Nothing


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node DestructurePattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\(Node nameRange name) afterStartName argsReverse ->
            let
                range : Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments = afterStartName |> Rope.prependTo argsReverse.comments
            , syntax =
                Node range
                    (NamedPattern_
                        name
                        (List.reverse argsReverse.syntax)
                    )
            }
        )
        qualifiedNameRefNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        { comments = arg.comments |> Rope.prependTo commentsAfterArg
                        , syntax = arg.syntax
                        }
                    )
                    patternNotDirectlyComposing
                    Layout.optimisticLayout
                )
            )
        )


qualifiedNameRefNode : Parser (Node QualifiedNameRef)
qualifiedNameRefNode =
    ParserFast.map2WithRange
        (\range firstName after ->
            Node range
                (case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
                )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node DestructurePattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
            , syntax =
                Node range
                    (NamedPattern_
                        (case after of
                            Nothing ->
                                { moduleName = [], name = firstName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = firstName :: qualificationAfter, name = unqualified }
                        )
                        []
                    )
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


recordPattern : Parser (WithComments (Node DestructurePattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> Rope.prependTo elements.comments
            , syntax =
                Node range (RecordPattern_ elements.syntax)
            }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    { comments =
                        commentsAfterHead
                            |> Rope.prependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                { comments = beforeName |> Rope.prependTo afterName
                                , syntax = name
                                }
                            )
                            Layout.maybeLayout
                            Tokens.functionNameNode
                            Layout.maybeLayout
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" { comments = Rope.empty, syntax = [] })
        )
