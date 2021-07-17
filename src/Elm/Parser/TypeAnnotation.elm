module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (..)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (..)


type Mode
    = Eager
    | Lazy


typeAnnotation : Parser State (Node Range (TypeAnnotation Range))
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


typeAnnotationNonGreedy : Parser State (Node Range (TypeAnnotation Range))
typeAnnotationNonGreedy =
    choice
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Parser State (Node Range (TypeAnnotation Range))
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


parensTypeAnnotation : Parser State (Node Range (TypeAnnotation Range))
parensTypeAnnotation =
    lazy
        (\() ->
            let
                commaSep : Parser State (List (Node Range (TypeAnnotation Range)))
                commaSep =
                    many
                        (string ","
                            |> Combine.ignore (maybe Layout.layout)
                            |> Combine.continueWith typeAnnotation
                            |> Combine.ignore (maybe Layout.layout)
                        )

                nested : Parser State (TypeAnnotation Range)
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
                            [ Combine.string ")" |> Combine.map (always (Tuple []))
                            , nested |> Combine.ignore (Combine.string ")")
                            ]
                        )
                )
        )


asTypeAnnotation : Node Range (TypeAnnotation Range) -> List (Node Range (TypeAnnotation Range)) -> TypeAnnotation Range
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            Tuple (x :: xs)


genericTypeAnnotation : Parser State (Node Range (TypeAnnotation Range))
genericTypeAnnotation =
    lazy
        (\() ->
            Node.parser (Combine.map Var functionName)
        )


recordFieldsTypeAnnotation : Parser State (List (Node Range (RecordField Range)))
recordFieldsTypeAnnotation =
    lazy (\() -> sepBy (string ",") (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition))


recordTypeAnnotation : Parser State (Node Range (TypeAnnotation Range))
recordTypeAnnotation =
    lazy
        (\() ->
            let
                nextField : Parser State (RecordField Range)
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

                additionalRecordFields : List (Node Range (RecordField Range)) -> Parser State (List (Node Range (RecordField Range)))
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
                                            [ Combine.string "|"
                                                |> Combine.continueWith recordFieldsTypeAnnotation
                                                |> Combine.ignore (Combine.string "}")
                                                |> Combine.andThen
                                                    (\fields ->
                                                        case fields of
                                                            head :: rest ->
                                                                ExtensionRecord fname head rest |> Combine.succeed

                                                            [] ->
                                                                Combine.fail "Extension records must have at least one field."
                                                    )
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


recordFieldDefinition : Parser State (RecordField Range)
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


typedTypeAnnotation : Mode -> Parser State (Node Range (TypeAnnotation Range))
typedTypeAnnotation mode =
    lazy
        (\() ->
            let
                genericHelper : List (Node Range (TypeAnnotation Range)) -> Parser State (List (Node Range (TypeAnnotation Range)))
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
                            (\() -> Combine.succeed (Node tir (Type original [])))
                            (\() ->
                                case mode of
                                    Eager ->
                                        Combine.map
                                            (\args ->
                                                Node
                                                    (Range.combine (tir :: nodeRanges args))
                                                    (Type original args)
                                            )
                                            (genericHelper [])

                                    Lazy ->
                                        Combine.succeed (Node tir (Type original []))
                            )
                    )
        )
