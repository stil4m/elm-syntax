module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import Parser as Core
import Parser.Extra


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    Combine.map
        (\ta ->
            \afterTa ->
                case afterTa of
                    Nothing ->
                        ta

                    Just out ->
                        Node.combine TypeAnnotation.FunctionTypeAnnotation ta out
        )
        (Combine.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |> Combine.keep
            (Combine.maybe
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.ignoreEntirely Tokens.arrowRight
                    |> Combine.continueWith Layout.maybeLayout
                    |> Combine.continueWith (Combine.lazy (\() -> typeAnnotation))
                )
            )


typeAnnotationNoFnExcludingTypedWithArguments : Parser State (Node TypeAnnotation)
typeAnnotationNoFnExcludingTypedWithArguments =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


typeAnnotationNoFnIncludingTypedWithArguments : Parser State (Node TypeAnnotation)
typeAnnotationNoFnIncludingTypedWithArguments =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


parensTypeAnnotation : Parser State TypeAnnotation
parensTypeAnnotation =
    Tokens.parensStart
        |> Combine.fromCoreContinue
            (Combine.oneOf
                [ Tokens.parensEnd
                    |> Combine.fromCoreMap (\() -> TypeAnnotation.Tuple [])
                , Layout.maybeLayout
                    |> Combine.continueWith
                        (Combine.map
                            (\firstPart ->
                                \lastToSecondPart ->
                                    case lastToSecondPart of
                                        [] ->
                                            let
                                                (Node _ firstPartValue) =
                                                    firstPart
                                            in
                                            firstPartValue

                                        _ ->
                                            TypeAnnotation.Tuple (firstPart :: List.reverse lastToSecondPart)
                            )
                            typeAnnotation
                        )
                    |> Combine.ignore Layout.maybeLayout
                    |> Combine.keep
                        (Combine.manyWithoutReverse
                            (Tokens.comma
                                |> Combine.fromCoreContinue Layout.maybeLayout
                                |> Combine.continueWith typeAnnotation
                                |> Combine.ignore Layout.maybeLayout
                            )
                        )
                    |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )


genericTypeAnnotation : Parser state TypeAnnotation
genericTypeAnnotation =
    Tokens.functionName
        |> Combine.fromCoreMap TypeAnnotation.Var


recordTypeAnnotation : Parser State TypeAnnotation
recordTypeAnnotation =
    Tokens.curlyStart
        |> Combine.fromCoreContinue Layout.maybeLayout
        |> Combine.continueWith
            (Combine.maybeMap identity
                (TypeAnnotation.Record [])
                (Node.parserCoreMap
                    (\firstName ->
                        \afterFirstName ->
                            case afterFirstName of
                                RecordExtensionExpressionAfterName fields ->
                                    TypeAnnotation.GenericRecord firstName fields

                                FieldsAfterName fieldsAfterName ->
                                    TypeAnnotation.Record (Node.combine Tuple.pair firstName fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                    )
                    Tokens.functionName
                    |> Combine.fromCoreIgnore Layout.maybeLayout
                    |> Combine.keep
                        (Combine.oneOf
                            [ Tokens.pipe
                                |> Combine.continueWithFromCore
                                    (Node.parserMap
                                        RecordExtensionExpressionAfterName
                                        recordFieldsTypeAnnotation
                                    )
                            , Tokens.colon
                                |> Combine.fromCoreIgnore Layout.maybeLayout
                                |> Combine.continueWith
                                    (typeAnnotation
                                        |> Combine.map
                                            (\firstFieldValue ->
                                                \tailFields ->
                                                    FieldsAfterName { firstFieldValue = firstFieldValue, tailFields = tailFields }
                                            )
                                    )
                                |> Combine.ignore Layout.maybeLayout
                                |> Combine.keep
                                    (Combine.maybeMap identity
                                        []
                                        (Tokens.comma
                                            |> Combine.fromCoreContinue recordFieldsTypeAnnotation
                                        )
                                    )
                            ]
                        )
                )
            )
        |> Combine.ignoreEntirely Tokens.curlyEnd


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 ","
        (Layout.maybeLayout
            |> Combine.continueWith (Node.parser recordFieldDefinition)
        )


recordFieldDefinition : Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Layout.maybeLayout
        |> Combine.continueWithCore
            (Node.parserCoreMap (\functionName -> \value -> ( functionName, value ))
                Tokens.functionName
            )
        |> Combine.ignore Layout.maybeLayout
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore Layout.maybeLayout
        |> Combine.keep typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |> Combine.ignore Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser State TypeAnnotation
typedTypeAnnotationWithoutArguments =
    Combine.fromCoreMap
        (\original -> TypeAnnotation.Type original [])
        typeIndicator


typeIndicator : Core.Parser (Node ( ModuleName, String ))
typeIndicator =
    Tokens.typeName
        |> Core.andThen (\typeOrSegment -> typeIndicatorHelper [] typeOrSegment)
        |> Node.parserCore


typeIndicatorHelper : ModuleName -> String -> Core.Parser ( ModuleName, String )
typeIndicatorHelper moduleNameSoFar typeOrSegment =
    Core.oneOf
        [ Tokens.dot
            |> Parser.Extra.continueWith Tokens.typeName
            |> Core.andThen (\t -> typeIndicatorHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed ( List.reverse moduleNameSoFar, typeOrSegment ))
        ]


typedTypeAnnotationWithArguments : Parser State TypeAnnotation
typedTypeAnnotationWithArguments =
    Core.map (\qualified -> \args -> TypeAnnotation.Type qualified args)
        typeIndicator
        |> Combine.fromCoreKeep
            (Combine.many
                (Layout.maybeLayout
                    |> Combine.backtrackable
                    |> Combine.continueWith typeAnnotationNoFnExcludingTypedWithArguments
                )
            )
