module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import CustomParser exposing (Parser)
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
        (CustomParser.orSucceed
            (CustomParser.map3
                (\commentsBeforeArrow commentsAfterArrow typeAnnotationResult ->
                    Just
                        { comments =
                            commentsBeforeArrow
                                |> Rope.prependTo commentsAfterArrow
                                |> Rope.prependTo typeAnnotationResult.comments
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy "->"
                    |> CustomParser.backtrackable
                )
                Layout.maybeLayout
                (CustomParser.lazy (\() -> typeAnnotation))
            )
            Nothing
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
    CustomParser.symbolFollowedBy "("
        (CustomParser.oneOf2
            (CustomParser.symbol ")" { comments = Rope.empty, syntax = TypeAnnotation.Unit })
            (CustomParser.map4
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
                    (CustomParser.map3
                        (\commentsBefore typeAnnotationResult commentsAfter ->
                            { comments =
                                commentsBefore
                                    |> Rope.prependTo typeAnnotationResult.comments
                                    |> Rope.prependTo commentsAfter
                            , syntax = typeAnnotationResult.syntax
                            }
                        )
                        (CustomParser.symbolFollowedBy "," Layout.maybeLayout)
                        typeAnnotation
                        Layout.maybeLayout
                    )
                )
            )
        )
        |> Node.parser


genericTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
genericTypeAnnotation =
    Tokens.functionName
        |> CustomParser.mapWithStartAndEndPosition
            (\start var end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end }
                        (TypeAnnotation.GenericType var)
                }
            )


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    CustomParser.map2
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
        (CustomParser.symbolFollowedBy "{" Layout.maybeLayout)
        (CustomParser.oneOf2
            (CustomParser.map4
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
                (Node.parserCore Tokens.functionName)
                Layout.maybeLayout
                (CustomParser.oneOf2
                    (CustomParser.map
                        (\extension ->
                            { comments = extension.comments
                            , syntax = RecordExtensionExpressionAfterName extension.syntax
                            }
                        )
                        (CustomParser.symbolFollowedBy "|"
                            (Node.parser recordFieldsTypeAnnotation)
                        )
                    )
                    (CustomParser.map4
                        (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
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
                        (CustomParser.symbolFollowedBy ":" Layout.maybeLayout)
                        typeAnnotation
                        Layout.maybeLayout
                        (CustomParser.orSucceed
                            (CustomParser.symbolFollowedBy "," recordFieldsTypeAnnotation)
                            { comments = Rope.empty, syntax = [] }
                        )
                    )
                )
                Tokens.curlyEnd
            )
            (CustomParser.symbol "}" Nothing)
        )
        |> Node.parser


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
    CustomParser.map6
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
        (Node.parserCore Tokens.functionName)
        (Layout.maybeLayoutUntilIgnored CustomParser.symbolFollowedBy ":")
        Layout.maybeLayout
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    CustomParser.mapWithStartAndEndPosition
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
        (CustomParser.map2
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


maybeDotTypeNamesTuple : CustomParser.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    CustomParser.orSucceed
        (CustomParser.map2
            (\firstName afterFirstName ->
                case afterFirstName of
                    Nothing ->
                        Just ( [], firstName )

                    Just ( qualificationAfter, unqualified ) ->
                        Just ( firstName :: qualificationAfter, unqualified )
            )
            (CustomParser.symbolFollowedBy "." Tokens.typeName)
            (CustomParser.lazy (\() -> maybeDotTypeNamesTuple))
        )
        Nothing


typedTypeAnnotationWithArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithArguments =
    CustomParser.map2
        (\nameNode args ->
            { comments = args.comments
            , syntax = TypeAnnotation.Typed nameNode args.syntax
            }
        )
        (CustomParser.mapWithStartAndEndPosition
            (\nameStart name nameEnd ->
                Node { start = nameStart, end = nameEnd }
                    name
            )
            (CustomParser.map2
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
        |> Node.parser
