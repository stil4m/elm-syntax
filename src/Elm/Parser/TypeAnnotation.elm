module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (..)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (..)


type Mode
    = Eager
    | Lazy


typeAnnotation : Parser State (Node TypeAnnotation)
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
                                    (Combine.map (\ta -> Node.combine FunctionTypeAnnotation typeRef ta)
                                        (string "->"
                                            |> Combine.ignore (maybe Layout.layout)
                                            |> Combine.continueWith typeAnnotation
                                        )
                                    )
                                    (succeed typeRef)
                            )
                    )
        )


typeAnnotationNonGreedy : Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    choice
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    lazy
        (\() ->
            choice
                [ parensTypeAnnotation
                , typedTypeAnnotation mode
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    lazy
        (\() ->
            let
                commaSep : Parser State (List (Node TypeAnnotation))
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
            Node.parser
                (Combine.string "("
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.string ")" |> Combine.map (always Unit)
                            , nested |> Combine.ignore (Combine.string ")")
                            ]
                        )
                )
        )


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            Tupled (x :: xs)


genericTypeAnnotation : Parser State (Node TypeAnnotation)
genericTypeAnnotation =
    lazy
        (\() ->
            Node.parser (Combine.map GenericType functionName)
        )


recordFieldsTypeAnnotation : Parser State RecordDefinition
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition))


recordTypeAnnotation : Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    lazy
        (\() ->
            let
                nextField : Parser State RecordField
                nextField =
                    Combine.succeed (\a b -> ( a, b ))
                        |> Combine.ignore (Combine.string ",")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap (Node.parser functionName)
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.ignore (string ":")
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andMap typeAnnotation
                        |> Combine.ignore Layout.optimisticLayout

                additionalRecordFields : RecordDefinition -> Parser State RecordDefinition
                additionalRecordFields items =
                    Combine.choice
                        [ Node.parser nextField
                            |> Combine.andThen (\next -> additionalRecordFields (next :: items))
                        , Combine.succeed (List.reverse items)
                        ]
            in
            Node.parser
                (string "{"
                    |> Combine.ignore (maybe Layout.layout)
                    |> Combine.continueWith
                        (Combine.choice
                            [ Combine.string "}" |> Combine.continueWith (Combine.succeed (Record []))
                            , Node.parser functionName
                                |> Combine.ignore (maybe Layout.layout)
                                |> Combine.andThen
                                    (\fname ->
                                        Combine.choice
                                            [ Combine.succeed (GenericRecord fname)
                                                |> Combine.ignore (Combine.string "|")
                                                |> Combine.andMap (Node.parser recordFieldsTypeAnnotation)
                                                |> Combine.ignore (Combine.string "}")
                                            , Combine.string ":"
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.continueWith typeAnnotation
                                                |> Combine.ignore (maybe Layout.layout)
                                                |> Combine.andThen
                                                    (\ta ->
                                                        additionalRecordFields [ Node.combine Tuple.pair fname ta ]
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
                |> Combine.andMap (maybe Layout.layout |> Combine.continueWith (Node.parser functionName))
                |> Combine.andMap
                    (maybe Layout.layout
                        |> Combine.continueWith (string ":")
                        |> Combine.continueWith (maybe Layout.layout)
                        |> Combine.continueWith typeAnnotation
                    )
        )


typedTypeAnnotation : Mode -> Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
    lazy
        (\() ->
            let
                genericHelper : List (Node TypeAnnotation) -> Parser State (List (Node TypeAnnotation))
                genericHelper items =
                    or
                        (typeAnnotationNoFn Lazy
                            |> Combine.andThen
                                (\next ->
                                    Layout.optimisticLayoutWith
                                        (\() -> Combine.succeed (List.reverse (next :: items)))
                                        (\() -> genericHelper (next :: items))
                                        |> Combine.ignore (maybe Layout.layout)
                                )
                        )
                        (Combine.succeed (List.reverse items))

                nodeRanges =
                    List.map (\(Node r _) -> r)
            in
            Node.parser typeIndicator
                |> Combine.andThen
                    (\((Node tir _) as original) ->
                        Layout.optimisticLayoutWith
                            (\() -> Combine.succeed (Node tir (Typed original [])))
                            (\() ->
                                case mode of
                                    Eager ->
                                        Combine.map
                                            (\args ->
                                                Node
                                                    (Range.combine (tir :: nodeRanges args))
                                                    (Typed original args)
                                            )
                                            (genericHelper [])

                                    Lazy ->
                                        Combine.succeed (Node tir (Typed original []))
                            )
                    )
        )
