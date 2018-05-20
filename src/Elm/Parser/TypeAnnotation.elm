module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (..)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Ranges exposing (ranged)
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName, typeName)
import Elm.Parser.Whitespace exposing (realNewLine)
import Elm.Syntax.Range as Range
import Elm.Syntax.Ranged exposing (Ranged)
import Elm.Syntax.TypeAnnotation exposing (..)


type Mode
    = Eager
    | Lazy


typeAnnotationNoFn : Mode -> Parser State (Ranged TypeAnnotation)
typeAnnotationNoFn mode =
    lazy
        (\() ->
            ranged <|
                choice
                    [ parensTypeAnnotation
                    , typedTypeAnnotation mode
                    , recordTypeAnnotation
                    , genericRecordTypeAnnotation
                    , genericTypeAnnotation
                    ]
        )


typeAnnotation : Parser State (Ranged TypeAnnotation)
typeAnnotation =
    lazy
        (\() ->
            typeAnnotationNoFn Eager
                |> Combine.andThen
                    (\typeRef ->
                        Layout.optimisticLayoutWith
                            (\() -> succeed typeRef)
                            (\() ->
                                or
                                    (Combine.map (\ta -> ( Range.combine [ Tuple.first typeRef, Tuple.first ta ], FunctionTypeAnnotation typeRef ta ))
                                        (string "->"
                                            |> Combine.ignore (maybe Layout.layout)
                                            |> Combine.continueWith typeAnnotation
                                        )
                                    )
                                    (succeed typeRef)
                            )
                    )
        )


typeAnnotationNonGreedy : Parser State (Ranged TypeAnnotation)
typeAnnotationNonGreedy =
    choice
        [ parensTypeAnnotation
        , typedTypeAnnotation False
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Parser State (Ranged TypeAnnotation)
typeAnnotationNoFn =
    lazy
        (\() ->
            choice
                [ parensTypeAnnotation
                , typedTypeAnnotation True
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Parser State (Ranged TypeAnnotation)
parensTypeAnnotation =
    lazy
        (\v ->
            let
                commaSep : Parser State (List (Ranged TypeAnnotation))
                commaSep =
                    many
                        (string ","
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith typeAnnotation
                            |> Combine.ignore (maybe Layout.layout)
                        )

                nested : Parser State TypeAnnotation
                nested =
                    Combine.succeed asTypeAnnotation
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap typeAnnotation
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap commaSep
            in
            ranged
                (Combine.fromCore (Core.symbol "(")
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.fromCore (Core.symbol ")") |> Combine.map (always Unit)
                            , nested |> Combine.ignore (Combine.fromCore (Core.symbol ")"))
                            ]
                        )
                )
        )


asTypeAnnotation : Ranged TypeAnnotation -> List (Ranged TypeAnnotation) -> TypeAnnotation
asTypeAnnotation x xs =
    case xs of
        [] ->
            Tuple.second x

        _ ->
            Tupled (x :: xs)


genericTypeAnnotation : Parser State (Ranged TypeAnnotation)
genericTypeAnnotation =
    lazy
        (\() ->
            ranged (Combine.map GenericType functionName)
        )


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (Layout.maybeAroundBothSides recordFieldDefinition))


recordTypeAnnotation : Parser State (Ranged TypeAnnotation)
recordTypeAnnotation =
    lazy
        (\() ->
            let
                nextField : Parser State RecordField
                nextField =
                    Combine.succeed Tuple.pair
                        |> Combine.ignore (Combine.string ",")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap functionName
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string ":")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap typeAnnotation
                        |> Combine.ignore Layout.optimisticLayout

                additionalRecordFields : RecordDefinition -> Parser State RecordDefinition
                additionalRecordFields items =
                    Combine.choice
                        [ nextField
                            |> Combine.andThen (\next -> additionalRecordFields (next :: items))
                        , Combine.succeed (List.reverse items)
                        ]
            in
            ranged
                (string "{"
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.string "}" |> Combine.continueWith (Combine.succeed (Record []))
                            , functionName
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andThen
                                    (\fname ->
                                        Combine.choice
                                            [ Combine.succeed (GenericRecord fname)
                                                |> Combine.ignore (Combine.string "|")
                                                |> Combine.andMap recordFieldsTypeAnnotation
                                                |> Combine.ignore (Combine.string "}")
                                            , Combine.string ":"
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.continueWith typeAnnotation
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.andThen
                                                    (\ta ->
                                                        additionalRecordFields [ ( fname, ta ) ]
                                                            |> Combine.map Record
                                                    )
                                                |> Combine.ignore (Combine.string "}")
                                            ]
                                    )
                            ]
                        )
                )
        )


recordFieldDefinition : Parser State RecordField
recordFieldDefinition =
    lazy
        (\() ->
            succeed Tuple.pair
                |> Combine.andMap (maybe Layout.layout |> Combine.continueWith functionName)
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string ":")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith typeAnnotation
                    )
        )


typedTypeAnnotation : Mode -> Parser State (Ranged TypeAnnotation)
typedTypeAnnotation mode =
    lazy
        (\() ->
            let
                genericHelper : List (Ranged TypeAnnotation) -> Parser State (List (Ranged TypeAnnotation))
                genericHelper items =
                    or
                        (typeAnnotationNoFn
                            |> Combine.andThen
                                (\next ->
                                    Layout.optimisticLayoutWith
                                        (\() -> Combine.succeed (List.reverse (next :: items)))
                                        (\() -> genericHelper (next :: items))
                                        |> Combine.ignore (maybe Layout.layout)
                                )
                        )
                        (Combine.succeed (List.reverse items))
            in
            ranged typeIndicator
                |> Combine.andThen
                    (\( tir, ( m, fn ) ) ->
                        Layout.optimisticLayoutWith
                            (\() -> Combine.succeed ( tir, Typed m fn [] ))
                            (\() ->
                                case mode of
                                    Eager ->
                                        genericHelper []
                                            |> Combine.map
                                                (\args ->
                                                    ( Range.combine (tir :: List.map Tuple.first args)
                                                    , Typed m fn args
                                                    )
                                                )

                                    Lazy ->
                                        Combine.succeed ( tir, Typed m fn [] )
                            )
                    )
        )
