module Elm.Parser.DestructurePatterns exposing (destructurePattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)


subDestructurePattern : Parser (WithComments (Node DestructurePattern))
subDestructurePattern =
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
    ParserFast.oneOf
        [ ParserFast.map2
            (\commentsAfterAs name ->
                { comments = commentsAfterAs
                , syntax = PatternComposedWithAs name
                }
            )
            (ParserFast.keywordFollowedBy "as" Layout.maybeLayout)
            (Node.parserCore Tokens.functionName)
        , ParserFast.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments (Node DestructurePattern))
parensPattern =
    ParserFast.map2
        (\commentsBeforeHead contentResult ->
            { comments =
                commentsBeforeHead
                    |> Rope.prependTo contentResult.comments
            , syntax = contentResult.syntax
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
                                ParenthesizedPattern_ headResult.syntax

                            _ ->
                                TuplePattern_ (headResult.syntax :: tailResult.syntax)
                    }
                )
                subDestructurePattern
                Layout.maybeLayout
                (ParserWithComments.until
                    Tokens.parensEnd
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides subDestructurePattern)
                    )
                )
            )
            (ParserFast.symbol ")" { comments = Rope.empty, syntax = UnitPattern_ })
        )
        |> Node.parser


varPattern : Parser (WithComments (Node DestructurePattern))
varPattern =
    Tokens.functionName
        |> ParserFast.mapWithStartAndEndPosition
            (\start var end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end } (VarPattern_ var)
                }
            )


composablePattern : Parser (WithComments (Node DestructurePattern))
composablePattern =
    ParserFast.oneOf
        [ varPattern
        , qualifiedPatternWithConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]


destructurePattern : Parser (WithComments (Node DestructurePattern))
destructurePattern =
    ParserFast.oneOf
        [ varPattern
        , qualifiedPatternWithoutConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]


allPattern : Parser (WithComments (Node DestructurePattern))
allPattern =
    ParserFast.symbol "_" { comments = Rope.empty, syntax = AllPattern_ }
        |> Node.parser


unitPattern : Parser (WithComments (Node DestructurePattern))
unitPattern =
    ParserFast.symbol "()" { comments = Rope.empty, syntax = UnitPattern_ }
        |> Node.parser


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
        (\startName afterStartName args ->
            { comments = args.comments
            , syntax =
                NamedPattern_
                    (case afterStartName of
                        Nothing ->
                            { moduleName = [], name = startName }

                        Just ( qualificationAfter, unqualified ) ->
                            { moduleName = startName :: qualificationAfter, name = unqualified }
                    )
                    args.syntax
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
                (Layout.maybeLayout |> ParserFast.backtrackable)
                destructurePattern
            )
        )
        |> Node.parser


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node DestructurePattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.mapWithStartAndEndPosition
        (\start name end ->
            { comments = Rope.empty
            , syntax =
                Node { start = start, end = end } (NamedPattern_ name [])
            }
        )
        (ParserFast.map2
            (\firstName after ->
                case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )


recordPattern : Parser (WithComments (Node DestructurePattern))
recordPattern =
    ParserFast.map2
        (\commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = patternRecordEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = RecordPattern_ elements.syntax
                    }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map4
                (\head commentsAfterHead tail () ->
                    Just
                        { comments =
                            commentsAfterHead
                                |> Rope.prependTo tail.comments
                        , syntax = head :: tail.syntax
                        }
                )
                (Node.parserCore Tokens.functionName)
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.map3
                        (\beforeName name afterName ->
                            { comments = beforeName |> Rope.prependTo afterName
                            , syntax = name
                            }
                        )
                        (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                        (Node.parserCore Tokens.functionName)
                        Layout.maybeLayout
                    )
                )
                Tokens.curlyEnd
            )
            (ParserFast.symbol "}" Nothing)
        )
        |> Node.parser


patternRecordEmpty : DestructurePattern
patternRecordEmpty =
    RecordPattern_ []
