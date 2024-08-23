module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
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
                (Layout.maybeLayoutUntilIgnoredBacktrackable ParserFast.symbol "->")
                Layout.maybeLayout
                (ParserFast.lazy (\() -> typeAnnotation))
            )
            Nothing
        )


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithArguments
        genericTypeAnnotation
        recordTypeAnnotation


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


genericTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
genericTypeAnnotation =
    Tokens.functionName
        |> ParserFast.mapWithStartAndEndPosition
            (\start var end ->
                { comments = Rope.empty
                , syntax =
                    Node { start = start, end = end }
                        (TypeAnnotation.GenericType var)
                }
            )


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2WithStartAndEndPosition
        (\start commentsBefore afterCurly end ->
            case afterCurly of
                Nothing ->
                    { comments = commentsBefore
                    , syntax = Node { start = start, end = end } typeAnnotationRecordEmpty
                    }

                Just afterCurlyResult ->
                    { comments =
                        commentsBefore
                            |> Rope.prependTo afterCurlyResult.comments
                    , syntax = Node { start = start, end = end } afterCurlyResult.syntax
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
                (Node.parserCore Tokens.functionName)
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.mapWithStartAndEndPosition
                            (\start extension end ->
                                { comments = extension.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        (Node { start = start, end = end } extension.syntax)
                                }
                            )
                            recordFieldsTypeAnnotation
                        )
                    )
                    (ParserFast.map4
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
                        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
                        typeAnnotation
                        Layout.maybeLayout
                        (ParserFast.orSucceed
                            (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                            { comments = Rope.empty, syntax = [] }
                        )
                    )
                )
                Tokens.curlyEnd
            )
            (ParserFast.symbol "}" Nothing)
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
        (ParserFast.map2
            (\commentsBefore fields ->
                { comments = commentsBefore |> Rope.prependTo fields.comments
                , syntax = fields.syntax
                }
            )
            Layout.maybeLayout
            recordFieldDefinition
        )


recordFieldDefinition : Parser (WithComments (Node TypeAnnotation.RecordField))
recordFieldDefinition =
    ParserFast.map6WithStartAndEndPosition
        (\start commentsBeforeFunctionName name commentsAfterFunctionName commentsAfterColon value commentsAfterValue end ->
            { comments =
                commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo value.comments
                    |> Rope.prependTo commentsAfterValue
            , syntax = Node { start = start, end = end } ( name, value.syntax )
            }
        )
        Layout.maybeLayout
        (Node.parserCore Tokens.functionName)
        (Layout.maybeLayoutUntilIgnored ParserFast.symbol ":")
        Layout.maybeLayout
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    ParserFast.map2WithStartAndEndPosition
        (\start startName afterStartName end ->
            let
                range : Range
                range =
                    { start = start, end = end }

                name : ( ModuleName, String )
                name =
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
            in
            { comments = Rope.empty
            , syntax =
                Node range
                    (TypeAnnotation.Typed (Node range name) [])
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


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
    ParserFast.map2WithStartAndEndPosition
        (\start nameNode args end ->
            { comments = args.comments
            , syntax =
                Node { start = start, end = end }
                    (TypeAnnotation.Typed nameNode args.syntax)
            }
        )
        (ParserFast.map2WithStartAndEndPosition
            (\start startName afterStartName end ->
                let
                    name : ( ModuleName, String )
                    name =
                        case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                in
                Node { start = start, end = end } name
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )
        (ParserWithComments.many
            (ParserFast.map2
                (\commentsBefore typeAnnotationResult ->
                    { comments = commentsBefore |> Rope.prependTo typeAnnotationResult.comments
                    , syntax = typeAnnotationResult.syntax
                    }
                )
                Layout.maybeLayoutBacktrackable
                typeAnnotationNoFnExcludingTypedWithArguments
            )
        )
