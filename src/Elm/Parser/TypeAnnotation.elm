module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Parser as Core exposing ((|=))


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFnIncludingTypedWithArgumentsLazy
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> typeRef)
                    (\() ->
                        Combine.oneOf
                            [ arrowRightToTypeAnnotation
                                |> Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                            , Combine.succeed typeRef
                            ]
                    )
            )


arrowRightToTypeAnnotation : Parser State (Node TypeAnnotation)
arrowRightToTypeAnnotation =
    Tokens.arrowRight
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith typeAnnotation


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


typeAnnotationNoFnIncludingTypedWithArgumentsLazy : Parser State (Node TypeAnnotation)
typeAnnotationNoFnIncludingTypedWithArgumentsLazy =
    Combine.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments)


parensTypeAnnotation : Parser State TypeAnnotation
parensTypeAnnotation =
    Tokens.parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd
                    |> Combine.fromCoreMap (\() -> TypeAnnotation.Unit)
                , parensTypeAnnotationInnerNested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )


parensTypeAnnotationInnerCommaSep : Parser State (List (Node TypeAnnotation))
parensTypeAnnotationInnerCommaSep =
    Combine.many
        (Tokens.comma
            |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
            |> Combine.continueWith typeAnnotation
            |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        )


parensTypeAnnotationInnerNested : Parser State TypeAnnotation
parensTypeAnnotationInnerNested =
    Combine.map (\() -> \x -> \xs -> asTypeAnnotation x xs)
        (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep parensTypeAnnotationInnerCommaSep


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            TypeAnnotation.Tupled (x :: xs)


genericTypeAnnotation : Parser state TypeAnnotation
genericTypeAnnotation =
    Tokens.functionName
        |> Core.map TypeAnnotation.GenericType
        |> Combine.fromCore


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 "," (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition)


recordTypeAnnotation : Parser State TypeAnnotation
recordTypeAnnotation =
    Tokens.curlyStart
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.fromCoreMap (\() -> TypeAnnotation.Record []) Tokens.curlyEnd
                , Node.parserCore Tokens.functionName
                    |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen
                        (\fname ->
                            Combine.oneOf
                                [ Combine.map (\fields -> TypeAnnotation.GenericRecord fname fields)
                                    pipeRecordFieldsTypeAnnotationNodeCurlyEnd
                                , Combine.map (\ta -> \rest -> TypeAnnotation.Record <| Node.combine Tuple.pair fname ta :: rest)
                                    colonTypeAnnotationMaybeLayout
                                    |> Combine.keep maybeCommaRecordFieldsTypeAnnotationCurlyEnd
                                ]
                        )
                ]
            )


pipeRecordFieldsTypeAnnotationNodeCurlyEnd : Parser State (Node TypeAnnotation.RecordDefinition)
pipeRecordFieldsTypeAnnotationNodeCurlyEnd =
    Tokens.pipe
        |> Combine.continueWithFromCore
            (Node.parser recordFieldsTypeAnnotation
                |> Combine.ignoreEntirely Tokens.curlyEnd
            )


colonTypeAnnotationMaybeLayout : Parser State (Node TypeAnnotation)
colonTypeAnnotationMaybeLayout =
    Tokens.colon
        |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (typeAnnotation
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
            )


maybeCommaRecordFieldsTypeAnnotationCurlyEnd : Parser State TypeAnnotation.RecordDefinition
maybeCommaRecordFieldsTypeAnnotationCurlyEnd =
    Combine.oneOf
        [ -- Skip a comma and then look for at least 1 more field
          Tokens.comma
            |> Combine.continueFromCore recordFieldsTypeAnnotation
        , -- Single field record, so just end with no additional fields
          Combine.succeed []
        ]
        |> Combine.ignoreEntirely Tokens.curlyEnd


recordFieldDefinition : Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Combine.map (\() -> \functionName -> \value -> ( functionName, value ))
        (Combine.maybeIgnore Layout.layout)
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


typedTypeAnnotationWithoutArguments : Parser State TypeAnnotation
typedTypeAnnotationWithoutArguments =
    Combine.fromCoreMap
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
        [ dotTypeName
            |> Core.andThen (\t -> typeIndicatorHelper (typeOrSegment :: moduleNameSoFar) t)
        , Core.lazy (\() -> Core.succeed ( List.reverse moduleNameSoFar, typeOrSegment ))
        ]


dotTypeName : Core.Parser String
dotTypeName =
    Core.map (\() -> identity) Tokens.dot
        |= Tokens.typeName


typedTypeAnnotationWithArguments : Parser State TypeAnnotation
typedTypeAnnotationWithArguments =
    Core.map (\qualified -> \args -> TypeAnnotation.Typed qualified args)
        typeIndicator
        |> Combine.fromCoreKeep
            (Combine.many
                (Combine.maybeIgnore Layout.layout
                    |> Combine.backtrackable
                    |> Combine.continueWith typeAnnotationNoFnExcludingTypedWithArguments
                )
            )
