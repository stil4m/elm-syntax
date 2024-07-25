module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import Parser as Core exposing ((|.))
import Parser.Extra
import ParserWithComments exposing (ParserWithComments)


typeAnnotation : ParserWithComments (Node TypeAnnotation)
typeAnnotation =
    ParserWithComments.map
        (\ta ->
            \afterTa ->
                case afterTa of
                    Nothing ->
                        ta

                    Just out ->
                        Node.combine TypeAnnotation.FunctionTypeAnnotation ta out
        )
        (Core.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |> ParserWithComments.keep
            (ParserWithComments.maybe
                ((Layout.maybeLayout
                    |> Core.backtrackable
                 )
                    |. Tokens.arrowRight
                    |> ParserWithComments.continueWith Layout.maybeLayout
                    |> ParserWithComments.continueWith (Core.lazy (\() -> typeAnnotation))
                )
            )


typeAnnotationNoFnExcludingTypedWithArguments : ParserWithComments (Node TypeAnnotation)
typeAnnotationNoFnExcludingTypedWithArguments =
    Core.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


typeAnnotationNoFnIncludingTypedWithArguments : ParserWithComments (Node TypeAnnotation)
typeAnnotationNoFnIncludingTypedWithArguments =
    Core.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


parensTypeAnnotation : ParserWithComments TypeAnnotation
parensTypeAnnotation =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Core.oneOf
                [ Tokens.parensEnd
                    |> ParserWithComments.fromCoreMap (\() -> TypeAnnotation.Unit)
                , (Layout.maybeLayout
                    |> ParserWithComments.continueWith
                        (ParserWithComments.map
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
                                            TypeAnnotation.Tupled (firstPart :: List.reverse lastToSecondPart)
                            )
                            typeAnnotation
                        )
                    |> ParserWithComments.ignore Layout.maybeLayout
                    |> ParserWithComments.keep
                        (ParserWithComments.manyWithoutReverse
                            (Tokens.comma
                                |> Parser.Extra.continueWith Layout.maybeLayout
                                |> ParserWithComments.continueWith typeAnnotation
                                |> ParserWithComments.ignore Layout.maybeLayout
                            )
                        )
                  )
                    |. Tokens.parensEnd
                ]
            )


genericTypeAnnotation : ParserWithComments TypeAnnotation
genericTypeAnnotation =
    Tokens.functionName
        |> ParserWithComments.fromCoreMap TypeAnnotation.GenericType


recordTypeAnnotation : ParserWithComments TypeAnnotation
recordTypeAnnotation =
    (Tokens.curlyStart
        |> Parser.Extra.continueWith Layout.maybeLayout
        |> ParserWithComments.continueWith
            (ParserWithComments.maybeMap identity
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
                    |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
                    |> ParserWithComments.keep
                        (Core.oneOf
                            [ Tokens.pipe
                                |> Parser.Extra.continueWith
                                    (Node.parserMap
                                        RecordExtensionExpressionAfterName
                                        recordFieldsTypeAnnotation
                                    )
                            , Tokens.colon
                                |> ParserWithComments.fromCoreIgnore Layout.maybeLayout
                                |> ParserWithComments.continueWith
                                    (typeAnnotation
                                        |> ParserWithComments.map
                                            (\firstFieldValue ->
                                                \tailFields ->
                                                    FieldsAfterName { firstFieldValue = firstFieldValue, tailFields = tailFields }
                                            )
                                    )
                                |> ParserWithComments.ignore Layout.maybeLayout
                                |> ParserWithComments.keep
                                    (ParserWithComments.maybeMap identity
                                        []
                                        (Tokens.comma
                                            |> Parser.Extra.continueWith recordFieldsTypeAnnotation
                                        )
                                    )
                            ]
                        )
                )
            )
    )
        |. Tokens.curlyEnd


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : ParserWithComments TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (Layout.maybeLayout
            |> ParserWithComments.continueWith (Node.parser recordFieldDefinition)
        )


recordFieldDefinition : ParserWithComments TypeAnnotation.RecordField
recordFieldDefinition =
    (Layout.maybeLayout
        |> ParserWithComments.continueWithCore
            (Node.parserCoreMap (\functionName -> \value -> ( functionName, value ))
                Tokens.functionName
            )
        |> ParserWithComments.ignore Layout.maybeLayout
    )
        |. Tokens.colon
        |> ParserWithComments.ignore Layout.maybeLayout
        |> ParserWithComments.keep typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |> ParserWithComments.ignore Layout.maybeLayout


typedTypeAnnotationWithoutArguments : ParserWithComments TypeAnnotation
typedTypeAnnotationWithoutArguments =
    ParserWithComments.fromCoreMap
        (\original -> TypeAnnotation.Typed original [])
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


typedTypeAnnotationWithArguments : ParserWithComments TypeAnnotation
typedTypeAnnotationWithArguments =
    Core.map (\qualified -> \args -> TypeAnnotation.Typed qualified args)
        typeIndicator
        |> ParserWithComments.fromCoreKeep
            (ParserWithComments.many
                (Layout.maybeLayout
                    |> Core.backtrackable
                    |> ParserWithComments.continueWith typeAnnotationNoFnExcludingTypedWithArguments
                )
            )
