module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import CustomParser exposing (Parser)
import CustomParser.Extra
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    CustomParser.map2
        (\ta afterTa ->
            case afterTa of
                Nothing ->
                    ta

                Just out ->
                    { comments = ta.comments |> Rope.prependTo out.comments
                    , syntax = Node.combine TypeAnnotation.FunctionTypeAnnotation ta.syntax out.syntax
                    }
        )
        (CustomParser.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        (CustomParser.oneOf
            [ CustomParser.map3
                (\commentsBeforeArrow commentsAfterArrow typeAnnotationResult ->
                    Just
                        { comments =
                            commentsBeforeArrow
                                |> Rope.prependTo commentsAfterArrow
                                |> Rope.prependTo typeAnnotationResult.comments
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayoutUntilIgnored CustomParser.symbol "->" |> CustomParser.backtrackable)
                Layout.maybeLayout
                (CustomParser.lazy (\() -> typeAnnotation))
            , CustomParser.succeed Nothing
            ]
        )


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    CustomParser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    CustomParser.oneOf
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
        |> CustomParser.Extra.continueWith
            (CustomParser.oneOf
                [ Tokens.parensEnd
                    |> CustomParser.map (\() -> unitWithComments)
                , CustomParser.map4
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
                        (CustomParser.map4
                            (\() commentsBefore typeAnnotationResult commentsAfter ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo typeAnnotationResult.comments
                                        |> Rope.prependTo commentsAfter
                                , syntax = typeAnnotationResult.syntax
                                }
                            )
                            Tokens.comma
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                        )
                    )
                ]
            )


unitWithComments : WithComments TypeAnnotation
unitWithComments =
    { comments = Rope.empty, syntax = TypeAnnotation.Unit }


genericTypeAnnotation : Parser (WithComments TypeAnnotation)
genericTypeAnnotation =
    Tokens.functionName
        |> CustomParser.map (\var -> { comments = Rope.empty, syntax = TypeAnnotation.GenericType var })


recordTypeAnnotation : Parser (WithComments TypeAnnotation)
recordTypeAnnotation =
    CustomParser.map3
        (\() commentsBefore afterCurly ->
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
        Layout.maybeLayout
        (CustomParser.oneOf
            [ CustomParser.map4
                (\firstNameStart firstName commentsAfterFirstName afterFirstName ->
                    let
                        firstNameNode : Node String
                        firstNameNode =
                            Node.singleLineStringFrom firstNameStart
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

                                FieldsAfterName fieldsAfterName ->
                                    TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                        }
                )
                CustomParser.getPosition
                Tokens.functionName
                Layout.maybeLayout
                (CustomParser.oneOf
                    [ CustomParser.map2
                        (\() extension ->
                            { comments = extension.comments
                            , syntax = RecordExtensionExpressionAfterName extension.syntax
                            }
                        )
                        Tokens.pipe
                        (Node.parser recordFieldsTypeAnnotation)
                    , CustomParser.map5
                        (\() commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
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
                        Layout.maybeLayout
                        typeAnnotation
                        Layout.maybeLayout
                        (CustomParser.oneOf
                            [ Tokens.comma
                                |> CustomParser.Extra.continueWith recordFieldsTypeAnnotation
                            , CustomParser.succeed { comments = Rope.empty, syntax = [] }
                            ]
                        )
                    ]
                )
                |> CustomParser.ignore Tokens.curlyEnd
            , CustomParser.map (\() -> Nothing)
                Tokens.curlyEnd
            ]
        )


typeAnnotationRecordEmpty : TypeAnnotation
typeAnnotationRecordEmpty =
    TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (CustomParser.map2
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
    CustomParser.map7
        (\commentsBeforeFunctionName nameStart name commentsAfterFunctionName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo value.comments
                    |> Rope.prependTo commentsAfterValue
            , syntax =
                ( Node.singleLineStringFrom nameStart
                    name
                , value.syntax
                )
            }
        )
        Layout.maybeLayout
        CustomParser.getPosition
        Tokens.functionName
        (Layout.maybeLayoutUntilIgnored CustomParser.symbol ":")
        Layout.maybeLayout
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    CustomParser.map4
        (\nameStart startName afterStartName nameEndColumn ->
            { comments = Rope.empty
            , syntax =
                TypeAnnotation.Typed
                    (Node
                        { start = nameStart
                        , end = { row = nameStart.row, column = nameEndColumn }
                        }
                        (case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                        )
                    )
                    []
            }
        )
        CustomParser.getPosition
        Tokens.typeName
        maybeDotTypeNamesTuple
        CustomParser.getCol


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.oneOf
        [ CustomParser.map3
            (\() firstName afterFirstName ->
                case afterFirstName of
                    Nothing ->
                        Just ( [], firstName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( firstName :: qualificationAfter, unqualified )
            )
            Tokens.dot
            Tokens.typeName
            (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        , CustomParser.succeed Nothing
        ]


typedTypeAnnotationWithArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithArguments =
    CustomParser.map5
        (\nameStart startName afterStartName nameEndColumn args ->
            { comments = args.comments
            , syntax =
                TypeAnnotation.Typed
                    (Node
                        { start = nameStart
                        , end = { row = nameStart.row, column = nameEndColumn }
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
        CustomParser.getPosition
        Tokens.typeName
        maybeDotTypeNamesTuple
        CustomParser.getCol
        (ParserWithComments.many
            (CustomParser.map2
                (\commentsBefore typeAnnotationResult ->
                    { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                (Layout.maybeLayout |> CustomParser.backtrackable)
                typeAnnotationNoFnExcludingTypedWithArguments
            )
        )
