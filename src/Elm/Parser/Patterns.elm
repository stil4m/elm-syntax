module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    ParserFast.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
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
                        Node.combine Pattern.AsPattern x.syntax anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern x.syntax y
            }
        )
        composablePattern
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    ParserFast.oneOf2OrSucceed
        (ParserFast.map2
            (\commentsAfterAs name ->
                { comments = commentsAfterAs
                , syntax = PatternComposedWithAs name
                }
            )
            (ParserFast.keywordFollowedBy "as" Layout.maybeLayout)
            Tokens.functionNameNode
        )
        (ParserFast.map2
            (\commentsAfterCons patternResult ->
                { comments = patternResult.comments |> Rope.prependTo commentsAfterCons
                , syntax = PatternComposedWithCons patternResult.syntax
                }
            )
            (ParserFast.symbolFollowedBy "::" Layout.maybeLayout)
            pattern
        )
        { comments = Rope.empty, syntax = PatternComposedWithNothing () }


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeHead contentResult ->
            { comments =
                commentsBeforeHead
                    |> Rope.prependTo contentResult.comments
            , syntax = Node range contentResult.syntax
            }
        )
        (ParserFast.symbolFollowedBy "(" Layout.maybeLayout)
        -- yes, (  ) is a valid pattern but not a valid type or expression
        (ParserFast.oneOf2
            (ParserFast.map3
                (\headResult commentsAfterHead tailResult ->
                    { comments =
                        headResult.comments
                            |> Rope.prependTo commentsAfterHead
                            |> Rope.prependTo tailResult.comments
                    , syntax =
                        case tailResult.syntax of
                            [] ->
                                ParenthesizedPattern headResult.syntax

                            _ ->
                                TuplePattern (headResult.syntax :: tailResult.syntax)
                    }
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.until
                    Tokens.parensEnd
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
            )
            (ParserFast.symbol ")" { comments = Rope.empty, syntax = UnitPattern })
        )


varPattern : Parser (WithComments (Node Pattern))
varPattern =
    Tokens.functionNameMapWithRange
        (\range var ->
            { comments = Rope.empty
            , syntax = Node range (VarPattern var)
            }
        )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> { comments = Rope.empty, syntax = Node range (IntPattern n) })
        (\range n -> { comments = Rope.empty, syntax = Node range (HexPattern n) })


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty, syntax = Node range (CharPattern char) }
        )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = Node range patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = Node range (ListPattern elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "[" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    Just
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                                |> Rope.prependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    ParserFast.oneOf10
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        unitPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotDirectlyComposing : Parser (WithComments (Node Pattern))
patternNotDirectlyComposing =
    ParserFast.oneOf10
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        unitPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (Node Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            { comments = Rope.empty
            , syntax = Node range AllPattern
            }
        )


unitPattern : Parser (WithComments (Node Pattern))
unitPattern =
    ParserFast.symbolWithRange "()"
        (\range ->
            { comments = Rope.empty
            , syntax = Node range UnitPattern
            }
        )


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Node range (StringPattern string)
            }
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\startName afterStartName ->
            case afterStartName of
                Nothing ->
                    Just ( [], startName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( startName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3WithRange
        (\range startName afterStartName args ->
            { comments = args.comments
            , syntax =
                Node range
                    (NamedPattern
                        (case afterStartName of
                            Nothing ->
                                { moduleName = [], name = startName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = startName :: qualificationAfter, name = unqualified }
                        )
                        args.syntax
                    )
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple
        (ParserWithComments.many
            (ParserFast.map2
                (\commentsBefore arg ->
                    { comments = arg.comments |> Rope.prependTo commentsBefore
                    , syntax = arg.syntax
                    }
                )
                Layout.maybeLayoutBacktrackable
                patternNotDirectlyComposing
            )
        )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            { comments = Rope.empty
            , syntax =
                Node range
                    (NamedPattern
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


recordPattern : Parser (WithComments (Node Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = Node range patternRecordEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax =
                        Node range
                            (RecordPattern elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    Just
                        { comments =
                            commentsAfterHead
                                |> Rope.prependTo tail.comments
                        , syntax = head :: tail.syntax
                        }
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.map3
                        (\beforeName name afterName ->
                            { comments = beforeName |> Rope.prependTo afterName
                            , syntax = name
                            }
                        )
                        (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                        Tokens.functionNameNode
                        Layout.maybeLayout
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" Nothing)
        )


patternRecordEmpty : Pattern
patternRecordEmpty =
    RecordPattern []
