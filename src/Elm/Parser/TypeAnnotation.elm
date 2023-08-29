module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine exposing (..)
import Elm.Parser.Base exposing (typeIndicator)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node as Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


type Mode
    = Eager
    | Lazy


typeAnnotation : Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFn Eager
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> succeed typeRef)
                    (\() ->
                        or
                            (Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                                (string "->"
                                    |> Combine.ignore (maybe Layout.layout)
                                    |> Combine.continueWith typeAnnotation
                                )
                            )
                            (succeed typeRef)
                    )
            )


typeAnnotationNonGreedy : Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    oneOf
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    lazy
        (\() ->
            oneOf
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
                |> Combine.keep typeAnnotation
                |> Combine.ignore (maybe Layout.layout)
                |> Combine.keep commaSep
    in
    Node.parser
        (Combine.string "("
            |> Combine.continueWith
                (Combine.oneOf
                    [ Combine.string ")" |> Combine.map (always TypeAnnotation.Unit)
                    , nested |> Combine.ignore (Combine.string ")")
                    ]
                )
        )


asTypeAnnotation : Node TypeAnnotation -> List (Node TypeAnnotation) -> TypeAnnotation
asTypeAnnotation ((Node _ value) as x) xs =
    case xs of
        [] ->
            value

        _ ->
            TypeAnnotation.Tupled (x :: xs)


genericTypeAnnotation : Parser State (Node TypeAnnotation)
genericTypeAnnotation =
    Node.parser (Combine.map TypeAnnotation.GenericType functionName)


recordFieldsTypeAnnotation : Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    sepBy1 (string ",") (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition)


recordTypeAnnotation : Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    Node.parser
        (string "{"
            |> Combine.ignore (maybe Layout.layout)
            |> Combine.continueWith
                (Combine.oneOf
                    [ Combine.string "}" |> Combine.continueWith (Combine.succeed (TypeAnnotation.Record []))
                    , Node.parser functionName
                        |> Combine.ignore (maybe Layout.layout)
                        |> Combine.andThen
                            (\fname ->
                                Combine.oneOf
                                    [ Combine.succeed (TypeAnnotation.GenericRecord fname)
                                        |> Combine.ignore (Combine.string "|")
                                        |> Combine.keep (Node.parser recordFieldsTypeAnnotation)
                                        |> Combine.ignore (Combine.string "}")
                                    , Combine.string ":"
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.continueWith typeAnnotation
                                        |> Combine.ignore (maybe Layout.layout)
                                        |> Combine.andThen
                                            (\ta ->
                                                Combine.oneOf
                                                    [ -- Skip a comma and then look for at least 1 more field
                                                      string ","
                                                        |> Combine.continueWith recordFieldsTypeAnnotation
                                                    , -- Single field record, so just end with no additional fields
                                                      Combine.succeed []
                                                    ]
                                                    |> Combine.ignore (Combine.string "}")
                                                    |> Combine.map (\rest -> TypeAnnotation.Record <| Node.combine Tuple.pair fname ta :: rest)
                                            )
                                    ]
                            )
                    ]
                )
        )


recordFieldDefinition : Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    succeed Tuple.pair
        |> Combine.keep (maybe Layout.layout |> Combine.continueWith (Node.parser functionName))
        |> Combine.keep
            (maybe Layout.layout
                |> Combine.continueWith (string ":")
                |> Combine.continueWith (maybe Layout.layout)
                |> Combine.continueWith typeAnnotation
            )


typedTypeAnnotation : Mode -> Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
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
                (Combine.succeed ()
                    |> Combine.map (\() -> List.reverse items)
                )
    in
    Node.parser typeIndicator
        |> Combine.andThen
            (\((Node tir _) as original) ->
                Layout.optimisticLayoutWith
                    (\() -> Combine.succeed (Node tir (TypeAnnotation.Typed original [])))
                    (\() ->
                        case mode of
                            Eager ->
                                Combine.map
                                    (\args ->
                                        Node
                                            (Range.combine (tir :: List.map Node.range args))
                                            (TypeAnnotation.Typed original args)
                                    )
                                    (genericHelper [])

                            Lazy ->
                                Combine.succeed (Node tir (TypeAnnotation.Typed original []))
                    )
            )
