module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    ParserFast.map2
        (\ta afterTa ->
            case afterTa of
                Nothing ->
                    ta

                Just out ->
                    { comments = ta.comments |> Rope.prependTo out.comments
                    , syntax = Node.combine TypeAnnotation.FunctionTypeAnnotation ta.syntax out.syntax
                    }
        )
        (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        (ParserFast.orSucceed
            (ParserFast.map3
                (\commentsBeforeArrow commentsAfterArrow typeAnnotationResult ->
                    Just
                        { comments =
                            commentsBeforeArrow
                                |> Rope.prependTo commentsAfterArrow
                                |> Rope.prependTo typeAnnotationResult.comments
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayoutUntilIgnored Parser.token "->" |> Parser.backtrackable)
                |= Layout.maybeLayout
                |= Parser.lazy (\() -> typeAnnotation)
            , Parser.succeed Nothing
            ]


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    ParserFast.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    ParserFast.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


parensTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
parensTypeAnnotation =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbol ")" { comments = Rope.empty, syntax = TypeAnnotation.Unit })
            (ParserFast.map4
                (\commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    { comments =
                        commentsBeforeFirstPart
                            |> Rope.prependTo firstPart.comments
                            |> Rope.prependTo commentsAfterFirstPart
                            |> Rope.prependTo lastToSecondPart.comments
                    , syntax =
                        case lastToSecondPart.syntax of
                            [] ->
                                let
                                    (Node _ firstPartValue) =
                                        firstPart.syntax
                                in
                                firstPartValue

                            _ ->
                                TypeAnnotation.Tupled (firstPart.syntax :: List.reverse lastToSecondPart.syntax)
                    }
                )
                Layout.maybeLayout
                typeAnnotation
                Layout.maybeLayout
                (ParserWithComments.untilWithoutReverse
                    Tokens.parensEnd
                    (ParserFast.map3
                        (\commentsBefore typeAnnotationResult commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo typeAnnotationResult.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax = typeAnnotationResult.syntax
                            }
                        )
                        (ParserFast.symbolFollowedBy "," Layout.maybeLayout)
                        typeAnnotation
                        Layout.maybeLayout
                    )
                )
            )
        )
        |> Node.parser


parensTypeAnnotation : Parser (WithComments TypeAnnotation)
parensTypeAnnotation =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ Tokens.parensEnd
                    |> Parser.map (\() -> unitWithComments)
                , Parser.map
                    (\commentsBeforeFirstPart ->
                        \firstPart ->
                            \commentsAfterFirstPart ->
                                \lastToSecondPart ->
                                    { comments =
                                        commentsBeforeFirstPart
                                            |> Rope.prependTo firstPart.comments
                                            |> Rope.prependTo commentsAfterFirstPart
                                            |> Rope.prependTo lastToSecondPart.comments
                                    , syntax =
                                        case lastToSecondPart.syntax of
                                            [] ->
                                                let
                                                    (Node _ firstPartValue) =
                                                        firstPart.syntax
                                                in
                                                firstPartValue

                                            _ ->
                                                TypeAnnotation.Tupled (firstPart.syntax :: List.reverse lastToSecondPart.syntax)
                                    }
                    )
                    Layout.maybeLayout
                    |= typeAnnotation
                    |= Layout.maybeLayout
                    |= ParserWithComments.untilWithoutReverse Tokens.parensEnd
                        (Parser.map
                            (\() ->
                                \commentsBefore ->
                                    \typeAnnotationResult ->
                                        \commentsAfter ->
                                            { comments =
                                                commentsBefore
                                                    |> Rope.prependTo typeAnnotationResult.comments
                                                    |> Rope.prependTo commentsAfter
                                            , syntax = typeAnnotationResult.syntax
                                            }
                            )
                            Tokens.comma
                            |= Layout.maybeLayout
                            |= typeAnnotation
                            |= Layout.maybeLayout
                        )
                ]
            )


unitWithComments : WithComments TypeAnnotation
unitWithComments =
    { comments = Rope.empty, syntax = TypeAnnotation.Unit }


genericTypeAnnotation : Parser (WithComments TypeAnnotation)
genericTypeAnnotation =
    Tokens.functionName
        |> Parser.map (\var -> { comments = Rope.empty, syntax = TypeAnnotation.GenericType var })


recordTypeAnnotation : Parser (WithComments TypeAnnotation)
recordTypeAnnotation =
    Parser.map
        (\() ->
            \commentsBefore ->
                \afterCurly ->
                    case afterCurly of
                        Nothing ->
                            { comments = commentsBefore
                            , syntax = typeAnnotationRecordEmpty
                            }

                        Just afterCurlyResult ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo afterCurlyResult.comments
                            , syntax = afterCurlyResult.syntax
                            }
        )
        Tokens.curlyStart
        |= Layout.maybeLayout
        |= Parser.oneOf
            [ Parser.map
                (\( firstNameStartRow, firstNameStartColumn ) ->
                    \firstName ->
                        \commentsAfterFirstName ->
                            \afterFirstName ->
                                let
                                    firstNameNode : Node String
                                    firstNameNode =
                                        Node.singleLineStringFrom
                                            { row = firstNameStartRow, column = firstNameStartColumn }
                                            firstName
                                in
                                Just
                                    { comments =
                                        commentsAfterFirstName
                                            |> Rope.prependTo afterFirstName.comments
                                    , syntax =
                                        case afterFirstName.syntax of
                                            RecordExtensionExpressionAfterName fields ->
                                                TypeAnnotation.GenericRecord firstNameNode fields


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2
        (\commentsBefore afterCurly ->
            case afterCurly of
                Nothing ->
                    { comments = commentsBefore
                    , syntax = typeAnnotationRecordEmpty
                    }

                Just afterCurlyResult ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo afterCurlyResult.comments
                    , syntax = afterCurlyResult.syntax
                    }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map4
                (\firstNameNode commentsAfterFirstName afterFirstName () ->
                    Just
                        { comments =
                            commentsAfterFirstName
                                |> Rope.prependTo afterFirstName.comments
                        , syntax =
                            case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName fields ->
                                    TypeAnnotation.GenericRecord firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                        }
                )
                Parser.getPosition
                |= Tokens.functionName
                |= Layout.maybeLayout
                |= Parser.oneOf
                    [ Tokens.pipe
                        |> Parser.Extra.continueWith
                            (Node.parserMap
                                RecordExtensionExpressionAfterName
                                recordFieldsTypeAnnotation
                            )
                    , Parser.map
                        (\() ->
                            \commentsBeforeFirstFieldValue ->
                                \firstFieldValue ->
                                    \commentsAfterFirstFieldValue ->
                                        \tailFields ->
                                            { comments =
                                                commentsBeforeFirstFieldValue
                                                    |> Rope.prependTo firstFieldValue.comments
                                                    |> Rope.prependTo commentsAfterFirstFieldValue
                                                    |> Rope.prependTo tailFields.comments
                                            , syntax =
                                                FieldsAfterName
                                                    { firstFieldValue = firstFieldValue.syntax
                                                    , tailFields = tailFields.syntax
                                                    }
                                            }
                        )
                        Tokens.colon
                        |= Layout.maybeLayout
                        |= typeAnnotation
                        |= Layout.maybeLayout
                        |= Parser.oneOf
                            [ Tokens.comma
                                |> Parser.Extra.continueWith recordFieldsTypeAnnotation
                            , Parser.succeed { comments = Rope.empty, syntax = [] }
                            ]
                    ]
            , Parser.succeed Nothing
            ]
        |. Tokens.curlyEnd


typeAnnotationRecordEmpty : TypeAnnotation
typeAnnotationRecordEmpty =
    TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (ParserFast.map2
            (\commentsBefore fields ->
                { comments = commentsBefore |> Rope.prependTo fields.comments
                , syntax = fields.syntax
                }
            )
            Layout.maybeLayout
            (Node.parser recordFieldDefinition)
        )


recordFieldDefinition : Parser (WithComments TypeAnnotation.RecordField)
recordFieldDefinition =
    ParserFast.map6
        (\commentsBeforeFunctionName name commentsAfterFunctionName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo value.comments
                    |> Rope.prependTo commentsAfterValue
            , syntax = ( name, value.syntax )
            }
        )
        Layout.maybeLayout
        |= Parser.getPosition
        |= Tokens.functionName
        |= Layout.maybeLayoutUntilIgnored Parser.token ":"
        |= Layout.maybeLayout
        |= typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    ParserFast.mapWithStartAndEndPosition
        (\nameStart name nameEnd ->
            let
                range : Range
                range =
                    { start = nameStart, end = nameEnd }
            in
            { comments = Rope.empty
            , syntax =
                Node range
                    (TypeAnnotation.Typed (Node range name) [])
            }
        )
        (ParserFast.map2
            (\startName afterStartName ->
                case afterStartName of
                    Nothing ->
                        ( [], startName )


maybeDotTypeNamesTuple : Parser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    Parser.oneOf
        [ Parser.map
            (\() ->
                \firstName ->
                    \afterFirstName ->
                        case afterFirstName of
                            Nothing ->
                                Just ( [], firstName )

                            Just ( qualificationAfter, unqualified ) ->
                                Just ( firstName :: qualificationAfter, unqualified )
            )
            Tokens.dot
            |= Tokens.typeName
            |= Parser.lazy (\() -> maybeDotTypeNamesTuple)
        , Parser.succeed Nothing
        ]


typedTypeAnnotationWithArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithArguments =
    Parser.map
        (\( nameStartRow, nameStartColumn ) ->
            \startName ->
                \afterStartName ->
                    \nameEndColumn ->
                        \args ->
                            { comments = args.comments
                            , syntax =
                                TypeAnnotation.Typed
                                    (Node
                                        { start = { row = nameStartRow, column = nameStartColumn }
                                        , end = { row = nameStartRow, column = nameEndColumn }
                                        }
                                        (case afterStartName of
                                            Nothing ->
                                                ( [], startName )

                                            Just ( qualificationAfterStartName, unqualified ) ->
                                                ( startName :: qualificationAfterStartName, unqualified )
                                        )
                                    )
                                    args.syntax
                            }
        )
        Parser.getPosition
        |= Tokens.typeName
        |= maybeDotTypeNamesTuple
        |= Parser.getCol
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \typeAnnotationResult ->
                        { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= typeAnnotationNoFnExcludingTypedWithArguments
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.orSucceed
        (ParserFast.map2
            (\firstName afterFirstName ->
                case afterFirstName of
                    Nothing ->
                        Just ( [], firstName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( firstName :: qualificationAfter, unqualified )
            )
            (ParserFast.symbolFollowedBy "." Tokens.typeName)
            (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        )
        Nothing


typedTypeAnnotationWithArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithArguments =
    ParserFast.map2
        (\nameNode args ->
            { comments = args.comments
            , syntax = TypeAnnotation.Typed nameNode args.syntax
            }
        )
        (ParserFast.mapWithStartAndEndPosition
            (\nameStart name nameEnd ->
                Node { start = nameStart, end = nameEnd }
                    name
            )
            (ParserFast.map2
                (\startName afterStartName ->
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
                )
                Tokens.typeName
                maybeDotTypeNamesTuple
            )
        )
        (ParserWithComments.many
            (ParserFast.map2
                (\commentsBefore typeAnnotationResult ->
                    { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                (Layout.maybeLayout |> ParserFast.backtrackable)
                typeAnnotationNoFnExcludingTypedWithArguments
            )
        )
        |> Node.parser
