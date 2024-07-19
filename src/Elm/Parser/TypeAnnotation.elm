module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (Parser)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Parser as Core


type Mode
    = Eager
    | Lazy


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFn Eager
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> typeRef)
                    (\() ->
                        Combine.oneOf
                            [ Tokens.arrowRight
                                |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
                                |> Combine.continueWith typeAnnotation
                                |> Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                            , Combine.succeed typeRef
                            ]
                    )
            )


typeAnnotationNonGreedy : Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    Combine.oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    Combine.lazy
        (\() ->
            Combine.oneOf
                [ parensTypeAnnotation
                , typedTypeAnnotation mode
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    let
        commaSep : Parser State (List (Node TypeAnnotation))
        commaSep =
            Combine.many
                (Tokens.comma
                    |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
                    |> Combine.continueWith typeAnnotation
                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                )

        nested : Parser State TypeAnnotation
        nested =
            Combine.succeed (\x -> \xs -> asTypeAnnotation x xs)
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                |> Combine.keep typeAnnotation
                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                |> Combine.keep commaSep
    in
    Tokens.parensStart
        |> Combine.continueFromCore
            (Combine.oneOf
                [ Tokens.parensEnd
                    |> Core.map (\() -> TypeAnnotation.Unit)
                    |> Combine.fromCore
                , nested |> Combine.ignoreEntirely Tokens.parensEnd
                ]
            )
        |> Node.parser


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            TypeAnnotation.Tupled (x :: xs)


genericTypeAnnotation : Parser state (Node TypeAnnotation)
genericTypeAnnotation =
    Tokens.functionName
        |> Core.map TypeAnnotation.GenericType
        |> Node.parserFromCore


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 "," (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition)


recordTypeAnnotation : Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    Tokens.curlyStart
        |> Combine.continueFromCore (Combine.maybeIgnore Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.succeed (TypeAnnotation.Record [])
                    |> Combine.ignoreEntirely Tokens.curlyEnd
                , Node.parserCore Tokens.functionName
                    |> Combine.ignoreFromCore (Combine.maybeIgnore Layout.layout)
                    |> Combine.andThen
                        (\fname ->
                            Combine.oneOf
                                [ Combine.succeed (TypeAnnotation.GenericRecord fname)
                                    |> Combine.ignoreEntirely Tokens.pipe
                                    |> Combine.keep (Node.parser recordFieldsTypeAnnotation)
                                    |> Combine.ignoreEntirely Tokens.curlyEnd
                                , Combine.succeed (\ta -> \rest -> TypeAnnotation.Record <| Node.combine Tuple.pair fname ta :: rest)
                                    |> Combine.ignoreEntirely Tokens.colon
                                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                    |> Combine.keep typeAnnotation
                                    |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                                    |> Combine.keep
                                        (Combine.oneOf
                                            [ -- Skip a comma and then look for at least 1 more field
                                              Tokens.comma
                                                |> Combine.continueFromCore recordFieldsTypeAnnotation
                                            , -- Single field record, so just end with no additional fields
                                              Combine.succeed []
                                            ]
                                        )
                                    |> Combine.ignoreEntirely Tokens.curlyEnd
                                ]
                        )
                ]
            )
        |> Node.parser


recordFieldDefinition : Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Combine.succeed (\functionName -> \value -> ( functionName, value ))
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keepFromCore (Node.parserCore Tokens.functionName)
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.ignoreEntirely Tokens.colon
        |> Combine.ignore (Combine.maybeIgnore Layout.layout)
        |> Combine.keep typeAnnotation


typedTypeAnnotation : Mode -> Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
    typeIndicator
        |> Combine.fromCore
        |> Combine.andThen
            (\((Node tir _) as original) ->
                Layout.optimisticLayoutWith
                    (\() -> Node tir (TypeAnnotation.Typed original []))
                    (\() ->
                        case mode of
                            Eager ->
                                eagerTypedTypeAnnotation original

                            Lazy ->
                                Combine.succeed (Node tir (TypeAnnotation.Typed original []))
                    )
            )


eagerTypedTypeAnnotation : Node ( ModuleName, String ) -> Parser State (Node TypeAnnotation)
eagerTypedTypeAnnotation ((Node range _) as original) =
    let
        genericHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
        genericHelper items =
            Combine.oneOf
                [ typeAnnotationNoFn Lazy
                    |> Combine.andThen
                        (\next ->
                            Layout.optimisticLayoutWith
                                (\() -> next :: items)
                                (\() -> genericHelper (next :: items))
                                |> Combine.ignore (Combine.maybeIgnore Layout.layout)
                        )
                , Combine.succeed items
                ]
    in
    genericHelper []
        |> Combine.map
            (\args ->
                let
                    endRange : Range
                    endRange =
                        case args of
                            (Node argRange _) :: _ ->
                                argRange

                            [] ->
                                range
                in
                Node
                    { start = range.start, end = endRange.end }
                    (TypeAnnotation.Typed original (List.reverse args))
            )
