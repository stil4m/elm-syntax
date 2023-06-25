module Elm.Parser.DestructurePatterns exposing (destructurePattern)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.DestructurePattern as DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Node as Node exposing (Node)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)


destructurePattern : Parser (WithComments (Node DestructurePattern))
destructurePattern =
    Parser.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node DestructurePattern))
composablePatternTryToCompose =
    Parser.map
        (\x ->
            \commentsAfterLeft ->
                \maybeComposedWithResult ->
                    { comments =
                        x.comments
                            |> Rope.prependTo commentsAfterLeft
                            |> Rope.prependTo maybeComposedWithResult.comments
                    , syntax =
                        case maybeComposedWithResult.syntax of
                            PatternComposedWithNothing () ->
                                x.syntax

                            PatternComposedWithAs anotherName ->
                                Node.combine DestructurePattern.AsPattern_ x.syntax anotherName
                    }
        )
        composablePattern
        |= Layout.maybeLayout
        |= maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    Parser.oneOf
        [ (Tokens.asToken
            |> Parser.Extra.continueWith
                (Parser.map
                    (\commentsAfterAs ->
                        \( nameStartRow, nameStartColumn ) ->
                            \name ->
                                { comments = commentsAfterAs
                                , syntax =
                                    PatternComposedWithAs
                                        (Node.singleLineStringFrom
                                            { row = nameStartRow, column = nameStartColumn }
                                            name
                                        )
                                }
                    )
                    Layout.layout
                )
          )
            |= Parser.getPosition
            |= Tokens.functionName
        , Parser.succeed { comments = Rope.empty, syntax = PatternComposedWithNothing () }
        ]


parensPattern : Parser (WithComments DestructurePattern)
parensPattern =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBeforeHead ->
                    \contentResult ->
                        { comments =
                            commentsBeforeHead
                                |> Rope.prependTo contentResult.comments
                        , syntax = contentResult.syntax
                        }
                )
                -- yes, (  ) is a valid pattern but not a valid type or expression
                Layout.maybeLayout
                |= Parser.oneOf
                    [ Parser.map
                        (\headResult ->
                            \commentsAfterHead ->
                                \tailResult ->
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
                        destructurePattern
                        |= Layout.maybeLayout
                        |= ParserWithComments.until Tokens.parensEnd
                            (Tokens.comma |> Parser.Extra.continueWith (Layout.maybeAroundBothSides destructurePattern))
                    , Parser.map (\() -> unitPatternWithComments) Tokens.parensEnd
                    ]
            )


variablePart : Parser (WithComments DestructurePattern)
variablePart =
    Tokens.functionName
        |> Parser.map (\var -> { comments = Rope.empty, syntax = VarPattern_ var })


composablePattern : Parser (WithComments (Node DestructurePattern))
composablePattern =
    Parser.oneOf
        [ variablePart
        , qualifiedPatternWithConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]
        |> Node.parser


qualifiedPatternArg : Parser (WithComments (Node DestructurePattern))
qualifiedPatternArg =
    Parser.oneOf
        [ variablePart
        , qualifiedPatternWithoutConsumeArgs
        , allPattern
        , unitPattern
        , parensPattern
        , recordPattern
        ]
        |> Node.parser


allPattern : Parser (WithComments DestructurePattern)
allPattern =
    Parser.map (\() -> allPatternWithComments) (Parser.symbol "_")


allPatternWithComments : WithComments DestructurePattern
allPatternWithComments =
    { comments = Rope.empty, syntax = AllPattern_ }


unitPattern : Parser (WithComments DestructurePattern)
unitPattern =
    Parser.map (\() -> unitPatternWithComments) (Parser.symbol "()")


unitPatternWithComments : WithComments DestructurePattern
unitPatternWithComments =
    { comments = Rope.empty, syntax = UnitPattern_ }


maybeDotTypeNamesTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    Parser.oneOf
        [ (Tokens.dot
            |> Parser.Extra.continueWith
                (Parser.map
                    (\startName ->
                        \afterStartName ->
                            case afterStartName of
                                Nothing ->
                                    Just ( [], startName )

                                Just ( qualificationAfter, unqualified ) ->
                                    Just ( startName :: qualificationAfter, unqualified )
                    )
                    Tokens.typeName
                )
          )
            |= Parser.lazy (\() -> maybeDotTypeNamesTuple)
        , Parser.succeed Nothing
        ]


qualifiedPatternWithConsumeArgs : Parser (WithComments DestructurePattern)
qualifiedPatternWithConsumeArgs =
    Parser.map
        (\startName ->
            \afterStartName ->
                \args ->
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
        |= maybeDotTypeNamesTuple
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \arg ->
                        { comments = arg.comments |> Rope.prependTo commentsBefore
                        , syntax = arg.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= qualifiedPatternArg
            )


qualifiedPatternWithoutConsumeArgs : Parser (WithComments DestructurePattern)
qualifiedPatternWithoutConsumeArgs =
    Parser.map
        (\firstName ->
            \after ->
                case after of
                    Nothing ->
                        { comments = Rope.empty
                        , syntax = NamedPattern_ { moduleName = [], name = firstName } []
                        }

                    Just ( qualificationAfter, unqualified ) ->
                        { comments = Rope.empty
                        , syntax = NamedPattern_ { moduleName = firstName :: qualificationAfter, name = unqualified } []
                        }
        )
        Tokens.typeName
        |= maybeDotTypeNamesTuple


recordPattern : Parser (WithComments DestructurePattern)
recordPattern =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBeforeElements ->
                    \maybeElements ->
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
                Layout.maybeLayout
            )
    )
        |= Parser.oneOf
            [ Parser.map
                (\( headStartRow, headStartEnd ) ->
                    \head ->
                        \commentsAfterHead ->
                            \tail ->
                                Just
                                    { comments =
                                        commentsAfterHead
                                            |> Rope.prependTo tail.comments
                                    , syntax =
                                        Node.singleLineStringFrom
                                            { row = headStartRow, column = headStartEnd }
                                            head
                                            :: tail.syntax
                                    }
                )
                Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayout
                |= ParserWithComments.many
                    ((Tokens.comma
                        |> Parser.Extra.continueWith
                            (Parser.map
                                (\beforeName ->
                                    \( nameStartRow, nameStartColumn ) ->
                                        \name ->
                                            \afterName ->
                                                { comments = beforeName |> Rope.prependTo afterName
                                                , syntax =
                                                    Node.singleLineStringFrom
                                                        { row = nameStartRow, column = nameStartColumn }
                                                        name
                                                }
                                )
                                Layout.maybeLayout
                            )
                     )
                        |= Parser.getPosition
                        |= Tokens.functionName
                        |= Layout.maybeLayout
                    )
            , Parser.succeed Nothing
            ]
        |. Tokens.curlyEnd


patternRecordEmpty : DestructurePattern
patternRecordEmpty =
    RecordPattern_ []
