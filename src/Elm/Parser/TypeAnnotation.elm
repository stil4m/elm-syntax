module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    Parser.map
        (\ta ->
            \afterTa ->
                case afterTa of
                    Nothing ->
                        ta

                    Just out ->
                        { comments = Rope.flatFromList [ ta.comments, out.comments ]
                        , syntax = Node.combine TypeAnnotation.FunctionTypeAnnotation ta.syntax out.syntax
                        }
        )
        (Parser.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        |= Parser.oneOf
            [ Parser.map
                (\commentsBeforeArrow ->
                    \commentsAfterArrow ->
                        \typeAnnotationResult ->
                            Just
                                { comments =
                                    Rope.flatFromList
                                        [ commentsBeforeArrow
                                        , commentsAfterArrow
                                        , typeAnnotationResult.comments
                                        ]
                                , syntax = typeAnnotationResult.syntax
                                }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |. Tokens.arrowRight
                |= Layout.maybeLayout
                |= Parser.lazy (\() -> typeAnnotation)
            , Parser.succeed Nothing
            ]


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    Parser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithoutArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    Parser.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotationWithArguments
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]
        |> Node.parser


parensTypeAnnotation : Parser (WithComments TypeAnnotation)
parensTypeAnnotation =
    Tokens.parensStart
        |> Parser.Extra.continueWith
            (Parser.oneOf
                [ Tokens.parensEnd
                    |> Parser.map (\() -> unitWithComments)
                , (Parser.map
                    (\commentsBeforeFirstPart ->
                        \firstPart ->
                            \commentsAfterFirstPart ->
                                \lastToSecondPart ->
                                    { comments =
                                        Rope.flatFromList
                                            [ commentsBeforeFirstPart
                                            , firstPart.comments
                                            , commentsAfterFirstPart
                                            , lastToSecondPart.comments
                                            ]
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
                    |= ParserWithComments.manyWithoutReverse
                        ((Tokens.comma
                            |> Parser.Extra.continueWith
                                (Parser.map
                                    (\commentsBefore ->
                                        \typeAnnotationResult ->
                                            \commentsAfter ->
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsBefore
                                                        , typeAnnotationResult.comments
                                                        , commentsAfter
                                                        ]
                                                , syntax = typeAnnotationResult.syntax
                                                }
                                    )
                                    Layout.maybeLayout
                                )
                         )
                            |= typeAnnotation
                            |= Layout.maybeLayout
                        )
                  )
                    |. Tokens.parensEnd
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
    ((Tokens.curlyStart
        |> Parser.Extra.continueWith
            (Parser.map
                (\commentsBefore ->
                    \afterCurly ->
                        case afterCurly of
                            Nothing ->
                                { comments = commentsBefore
                                , syntax = TypeAnnotation.Record []
                                }

                            Just afterCurlyResult ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsBefore
                                        , afterCurlyResult.comments
                                        ]
                                , syntax = afterCurlyResult.syntax
                                }
                )
                Layout.maybeLayout
            )
     )
        |= Parser.oneOf
            [ Node.parserCoreMap
                (\firstName ->
                    \commentsAfterFirstName ->
                        \afterFirstName ->
                            Just
                                { comments =
                                    Rope.flatFromList
                                        [ commentsAfterFirstName
                                        , afterFirstName.comments
                                        ]
                                , syntax =
                                    case afterFirstName.syntax of
                                        RecordExtensionExpressionAfterName fields ->
                                            TypeAnnotation.GenericRecord firstName fields

                                        FieldsAfterName fieldsAfterName ->
                                            TypeAnnotation.Record (Node.combine Tuple.pair firstName fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                                }
                )
                Tokens.functionName
                |= Layout.maybeLayout
                |= Parser.oneOf
                    [ Tokens.pipe
                        |> Parser.Extra.continueWith
                            (Node.parserMap
                                RecordExtensionExpressionAfterName
                                recordFieldsTypeAnnotation
                            )
                    , (Tokens.colon
                        |> Parser.Extra.continueWith
                            (Parser.map
                                (\commentsBeforeFirstFieldValue ->
                                    \firstFieldValue ->
                                        \commentsAfterFirstFieldValue ->
                                            \tailFields ->
                                                { comments =
                                                    Rope.flatFromList
                                                        [ commentsBeforeFirstFieldValue
                                                        , firstFieldValue.comments
                                                        , commentsAfterFirstFieldValue
                                                        , tailFields.comments
                                                        ]
                                                , syntax =
                                                    FieldsAfterName
                                                        { firstFieldValue = firstFieldValue.syntax
                                                        , tailFields = tailFields.syntax
                                                        }
                                                }
                                )
                                Layout.maybeLayout
                            )
                      )
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
    )
        |. Tokens.curlyEnd


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserWithComments.sepBy1 ","
        (Parser.map
            (\commentsBefore ->
                \fields ->
                    { comments = Rope.flatFromList [ commentsBefore, fields.comments ]
                    , syntax = fields.syntax
                    }
            )
            Layout.maybeLayout
            |= Node.parser recordFieldDefinition
        )


recordFieldDefinition : Parser (WithComments TypeAnnotation.RecordField)
recordFieldDefinition =
    Parser.map
        (\commentsBeforeFunctionName ->
            \functionName ->
                \commentsAfterFunctionName ->
                    \commentsAfterColon ->
                        \value ->
                            \commentsAfterValue ->
                                { comments =
                                    Rope.flatFromList
                                        [ commentsBeforeFunctionName
                                        , commentsAfterFunctionName
                                        , commentsAfterColon
                                        , value.comments
                                        , commentsAfterValue
                                        ]
                                , syntax = ( functionName, value.syntax )
                                }
        )
        Layout.maybeLayout
        |= Node.parserCore Tokens.functionName
        |= Layout.maybeLayout
        |. Tokens.colon
        |= Layout.maybeLayout
        |= typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        |= Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithoutArguments =
    Parser.map
        (\original -> { comments = Rope.empty, syntax = TypeAnnotation.Typed original [] })
        typeIndicator


typeIndicator : Parser.Parser (Node ( ModuleName, String ))
typeIndicator =
    Tokens.typeName
        |> Parser.andThen (\typeOrSegment -> typeIndicatorHelper [] typeOrSegment)
        |> Node.parserCore


typeIndicatorHelper : ModuleName -> String -> Parser.Parser ( ModuleName, String )
typeIndicatorHelper moduleNameSoFar typeOrSegment =
    Parser.oneOf
        [ Tokens.dot
            |> Parser.Extra.continueWith Tokens.typeName
            |> Parser.andThen (\t -> typeIndicatorHelper (typeOrSegment :: moduleNameSoFar) t)
        , Parser.lazy (\() -> Parser.succeed ( List.reverse moduleNameSoFar, typeOrSegment ))
        ]


typedTypeAnnotationWithArguments : Parser (WithComments TypeAnnotation)
typedTypeAnnotationWithArguments =
    Parser.map
        (\qualified ->
            \args ->
                { comments = args.comments
                , syntax = TypeAnnotation.Typed qualified args.syntax
                }
        )
        typeIndicator
        |= ParserWithComments.many
            (Parser.map
                (\commentsBefore ->
                    \typeAnnotationResult ->
                        { comments = Rope.flatFromList [ commentsBefore, typeAnnotationResult.comments ]
                        , syntax = typeAnnotationResult.syntax
                        }
                )
                (Layout.maybeLayout |> Parser.backtrackable)
                |= typeAnnotationNoFnExcludingTypedWithArguments
            )
