module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNonGreedy)

import Combine
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


typeAnnotation : Combine.Parser State (Node TypeAnnotation)
typeAnnotation =
    typeAnnotationNoFn Eager
        |> Combine.andThen
            (\typeRef ->
                Layout.optimisticLayoutWith
                    (\() -> Combine.succeed typeRef)
                    (\() ->
                        Combine.or
                            (Combine.map (\ta -> Node.combine TypeAnnotation.FunctionTypeAnnotation typeRef ta)
                                (Combine.string "->"
                                    |> Combine.ignore (Combine.maybe Layout.layout)
                                    |> Combine.continueWith typeAnnotation
                                )
                            )
                            (Combine.succeed typeRef)
                    )
            )


typeAnnotationNonGreedy : Combine.Parser State (Node TypeAnnotation)
typeAnnotationNonGreedy =
    Combine.choice
        [ parensTypeAnnotation
        , typedTypeAnnotation Lazy
        , genericTypeAnnotation
        , recordTypeAnnotation
        ]


typeAnnotationNoFn : Mode -> Combine.Parser State (Node TypeAnnotation)
typeAnnotationNoFn mode =
    Combine.lazy
        (\() ->
            Combine.choice
                [ parensTypeAnnotation
                , typedTypeAnnotation mode
                , genericTypeAnnotation
                , recordTypeAnnotation
                ]
        )


parensTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
parensTypeAnnotation =
    let
        commaSep : Combine.Parser State (List (Node TypeAnnotation))
        commaSep =
            Combine.many
                (Combine.string ","
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.continueWith typeAnnotation
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )

        nested : Combine.Parser State TypeAnnotation
        nested =
            Combine.succeed asTypeAnnotation
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.andMap typeAnnotation
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.andMap commaSep
    in
    Node.parser
        (Combine.string "("
            |> Combine.continueWith
                (Combine.choice
                    [ Combine.string ")" |> Combine.map (always (TypeAnnotation.Tuple []))
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
            TypeAnnotation.Tuple (x :: xs)


genericTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
genericTypeAnnotation =
    Node.parser (Combine.map TypeAnnotation.Var functionName)


recordFieldsTypeAnnotation : Combine.Parser State TypeAnnotation.RecordDefinition
recordFieldsTypeAnnotation =
    Combine.sepBy1 (Combine.string ",") (Layout.maybeAroundBothSides <| Node.parser recordFieldDefinition)
        |> Combine.map (\( head, rest ) -> head :: rest)


recordTypeAnnotation : Combine.Parser State (Node TypeAnnotation)
recordTypeAnnotation =
    Node.parser
        (Combine.string "{"
            |> Combine.ignore (Combine.maybe Layout.layout)
            |> Combine.continueWith
                (Combine.choice
                    [ Combine.string "}" |> Combine.continueWith (Combine.succeed (TypeAnnotation.Record []))
                    , Node.parser functionName
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.andThen
                            (\fname ->
                                Combine.choice
                                    [ Combine.succeed (TypeAnnotation.GenericRecord fname)
                                        |> Combine.ignore (Combine.string "|")
                                        |> Combine.andMap (Node.parser recordFieldsTypeAnnotation)
                                        |> Combine.ignore (Combine.string "}")
                                    , Combine.string ":"
                                        |> Combine.ignore (Combine.maybe Layout.layout)
                                        |> Combine.continueWith typeAnnotation
                                        |> Combine.ignore (Combine.maybe Layout.layout)
                                        |> Combine.andThen
                                            (\ta ->
                                                Combine.choice
                                                    [ -- Skip a comma and then look for at least 1 more field
                                                      Combine.string ","
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


recordFieldDefinition : Combine.Parser State TypeAnnotation.RecordField
recordFieldDefinition =
    Combine.succeed Tuple.pair
        |> Combine.andMap (Combine.maybe Layout.layout |> Combine.continueWith (Node.parser functionName))
        |> Combine.andMap
            (Combine.maybe Layout.layout
                |> Combine.continueWith (Combine.string ":")
                |> Combine.continueWith (Combine.maybe Layout.layout)
                |> Combine.continueWith typeAnnotation
            )


typedTypeAnnotation : Mode -> Combine.Parser State (Node TypeAnnotation)
typedTypeAnnotation mode =
    let
        genericHelper : List (Node TypeAnnotation) -> Combine.Parser State (List (Node TypeAnnotation))
        genericHelper items =
            Combine.or
                (typeAnnotationNoFn Lazy
                    |> Combine.andThen
                        (\next ->
                            Layout.optimisticLayoutWith
                                (\() -> Combine.succeed (List.reverse (next :: items)))
                                (\() -> genericHelper (next :: items))
                                |> Combine.ignore (Combine.maybe Layout.layout)
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
                    (\() -> Combine.succeed (Node tir (TypeAnnotation.Type original [])))
                    (\() ->
                        case mode of
                            Eager ->
                                Combine.map
                                    (\args ->
                                        Node
                                            (Range.combine (tir :: nodeRanges args))
                                            (TypeAnnotation.Type original args)
                                    )
                                    (genericHelper [])

                            Lazy ->
                                Combine.succeed (Node tir (TypeAnnotation.Type original []))
                    )
            )
