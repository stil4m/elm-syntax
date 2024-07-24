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
    Combine.map (\ta -> \fromTa -> fromTa ta)
        (Combine.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |> Combine.keep
            (Combine.oneOf
                [ Combine.maybeIgnore Layout.layout
                    |> Combine.backtrackable
                    |> Combine.continueWithCore Tokens.arrowRight
                    |> Combine.continueWith (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith (Combine.lazy (\() -> typeAnnotation))
                    |> Combine.map (\out -> \in_ -> Node.combine TypeAnnotation.FunctionTypeAnnotation in_ out)
                , Combine.succeed identity
                ]
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
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd
                    |> Combine.fromCoreMap (\() -> TypeAnnotation.Unit)
                , parensTypeAnnotationInnerNested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )


parensTypeAnnotationInnerNested : Parser State TypeAnnotation
parensTypeAnnotationInnerNested =
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWith
            (Combine.map (\x -> \xs -> asTypeAnnotation x xs)
                typeAnnotation
            )
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep
            (Combine.many
                (Tokens.comma
                    |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith typeAnnotation
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )
            )


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
        |> Combine.fromCoreMap TypeAnnotation.GenericType


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 ","
        (Combine.maybeIgnore Layout.layout
            |> Combine.continueWith (Node.parser recordFieldDefinition)
        )


recordTypeAnnotation : Parser State TypeAnnotation
recordTypeAnnotation =
    Tokens.curlyStart
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.fromCoreMap (\() -> TypeAnnotation.Record []) Tokens.curlyEnd
                , Node.parserCoreMap (\fName -> \fromFName -> fromFName fName)
                    Tokens.functionName
                    |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
                    |> Combine.keep
                        (Combine.oneOf
                            [ Combine.map (\fields -> \fname -> TypeAnnotation.GenericRecord fname fields)
                                pipeRecordFieldsTypeAnnotationNodeCurlyEnd
                            , Core.map (\() -> \ta -> \rest -> \fname -> TypeAnnotation.Record (Node.combine Tuple.pair fname ta :: rest))
                                Tokens.colon
                                |> Combine.fromCoreIgnore (Combine.maybeIgnore Layout.layout)
                                |> Combine.keep
                                    (typeAnnotation
                                        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                    )
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
    Combine.maybeIgnore Layout.layout
        |> Combine.continueWithCore
            (Node.parserCoreMap (\functionName -> \value -> ( functionName, value ))
                Tokens.functionName
            )
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)


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
