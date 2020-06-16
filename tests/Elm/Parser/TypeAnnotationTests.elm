module Elm.Parser.TypeAnnotationTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.TypeAnnotation as Parser
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "TypeReferenceTests"
        [ test "unitTypeReference" <|
            \() ->
                parseFullStringWithNullState "()" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Node.empty (Tuple []))
        , test "unitTypeReference with spaces" <|
            \() ->
                parseFullStringWithNullState "( )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal Nothing
        , test "tupledTypeReference" <|
            \() ->
                parseFullStringWithNullState "( (), ())" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Node.empty <| Tuple [ Node.empty (Tuple []), Node.empty (Tuple []) ])
        , test "tupledTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "( () )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Node.empty (Tuple []))
        , test "tupledTypeReference 3" <|
            \() ->
                parseFullStringWithNullState "( () , Maybe m )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node.empty <|
                                Tuple
                                    [ Node.empty (Tuple [])
                                    , Node empty <| Typed (Node empty <| ( [], "Maybe" )) [ Node empty <| GenericType "m" ]
                                    ]
                            )
                        )
        , test "qualified type reference" <|
            \() ->
                parseFullStringWithNullState "Foo.Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Node empty <| Typed (Node empty ( [ "Foo" ], "Bar" )) []))
        , test "typeAnnotationNoFn" <|
            \() ->
                parseFullStringWithNullState "Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Node empty <| Typed (Node empty ( [], "Bar" )) []))
        , test "types with and without spacing should parse to the same" <|
            \() ->
                let
                    a =
                        parseFullStringWithNullState "Bar " Parser.typeAnnotation

                    b =
                        parseFullStringWithNullState "Bar" Parser.typeAnnotation
                in
                a
                    |> Expect.equal b
        , test "typedTypeReference 1" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                Typed (Node empty ( [], "Foo" ))
                                    [ Node empty (Tuple [])
                                    , Node empty <| GenericType "a"
                                    , Node empty <| Typed (Node empty ( [], "Bar" )) []
                                    ]
                            )
                        )
        , test "typedTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                Typed (Node empty ( [], "Foo" ))
                                    [ Node empty (Tuple [])
                                    , Node empty <| GenericType "a"
                                    , Node empty <| Typed (Node empty ( [], "Bar" )) []
                                    ]
                            )
                        )
        , test "recordTypeReference empty" <|
            \() ->
                parseFullStringWithNullState "{}" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                Record []
                        )
        , test "recordTypeReference one field" <|
            \() ->
                parseFullStringWithNullState "{color: String }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Node empty <|
                                Record [ Node empty ( Node empty "color", Node empty <| Typed (Node empty ( [], "String" )) [] ) ]
                        )
        , test "record with generic" <|
            \() ->
                parseFullStringWithNullState "{ attr | position : Vec2, texture : Vec2 }" Parser.typeAnnotation
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                                (GenericRecord
                                    (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 42 } }
                                        [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 25 } }
                                            ( Node { start = { row = 1, column = 10 }, end = { row = 1, column = 18 } } "position"
                                            , Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } }
                                                (Typed (Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } ( [], "Vec2" )) [])
                                            )
                                        , Node { start = { row = 1, column = 27 }, end = { row = 1, column = 42 } }
                                            ( Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } } "texture"
                                            , Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } }
                                                (Typed (Node { start = { row = 1, column = 37 }, end = { row = 1, column = 41 } } ( [], "Vec2" )) [])
                                            )
                                        ]
                                    )
                                )
                            )
                        )
        , test "generic record with no fields" <|
            \() ->
                parseFullStringWithNullState "{ attr |}" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal Nothing
        , test "recordTypeReference nested record" <|
            \() ->
                parseFullStringWithNullState "{color: {r : Int, g :Int, b: Int } }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                Record
                                    [ Node empty
                                        ( Node empty "color"
                                        , Node empty <|
                                            Record
                                                [ Node empty ( Node empty "r", Node empty <| Typed (Node empty ( [], "Int" )) [] )
                                                , Node empty ( Node empty "g", Node empty <| Typed (Node empty ( [], "Int" )) [] )
                                                , Node empty ( Node empty "b", Node empty <| Typed (Node empty ( [], "Int" )) [] )
                                                ]
                                        )
                                    ]
                            )
                        )
        , test "record field ranges" <|
            \() ->
                parseFullStringWithNullState "{ foo : Int, bar : Int, baz : Int }" Parser.typeAnnotation
                    |> Expect.equal
                        (Just <|
                            (Node { start = { column = 1, row = 1 }, end = { column = 36, row = 1 } } <|
                                Record
                                    [ Node { start = { column = 3, row = 1 }, end = { column = 12, row = 1 } }
                                        ( Node { start = { column = 3, row = 1 }, end = { column = 6, row = 1 } } "foo"
                                        , Node { start = { column = 9, row = 1 }, end = { column = 12, row = 1 } } <|
                                            Typed (Node { start = { column = 9, row = 1 }, end = { column = 12, row = 1 } } ( [], "Int" )) []
                                        )
                                    , Node { start = { column = 14, row = 1 }, end = { column = 23, row = 1 } }
                                        ( Node { start = { column = 14, row = 1 }, end = { column = 17, row = 1 } } "bar"
                                        , Node { start = { column = 20, row = 1 }, end = { column = 23, row = 1 } } <|
                                            Typed (Node { start = { column = 20, row = 1 }, end = { column = 23, row = 1 } } ( [], "Int" )) []
                                        )
                                    , Node { start = { column = 25, row = 1 }, end = { column = 35, row = 1 } }
                                        ( Node { start = { column = 25, row = 1 }, end = { column = 28, row = 1 } } "baz"
                                        , Node { start = { column = 31, row = 1 }, end = { column = 34, row = 1 } } <|
                                            Typed (Node { start = { column = 31, row = 1 }, end = { column = 34, row = 1 } } ( [], "Int" )) []
                                        )
                                    ]
                            )
                        )
        , test "recordTypeReference with generic" <|
            \() ->
                parseFullStringWithNullState "{color: s }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                Record
                                    [ Node empty
                                        ( Node empty "color"
                                        , Node empty <| GenericType "s"
                                        )
                                    ]
                            )
                        )
        , test "function type reference" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <| Typed (Node empty ( [], "Foo" )) [])
                                    (Node empty <| Typed (Node empty ( [], "Bar" )) [])
                            )
                        )
        , test "function type reference multiple" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <| Typed (Node empty ( [], "Foo" )) [])
                                    (Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <| Typed (Node empty ( [], "Bar" )) [])
                                            (Node empty <| GenericType "baz")
                                    )
                            )
                        )
        , test "function type reference generics" <|
            \() ->
                parseFullStringWithNullState "cMsg -> cModel -> a" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <| GenericType "cMsg")
                                    (Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <| GenericType "cModel")
                                            (Node empty <| GenericType "a")
                                    )
                            )
                        )
        , test "function with spacing on indent 0" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Model\n\nsomeFunction" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just (Node empty <| Typed (Node empty ( [], "Model" )) []))
        , test "annotation with parens" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Msg -> Model -> (Model, Cmd Msg)\n\n" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                FunctionTypeAnnotation (Node empty <| Typed (Node empty ( [], "Msg" )) [])
                                    (Node empty <|
                                        FunctionTypeAnnotation (Node empty <| Typed (Node empty ( [], "Model" )) [])
                                            (Node empty <|
                                                Tuple
                                                    [ Node empty <| Typed (Node empty ( [], "Model" )) []
                                                    , Node empty <| Typed (Node empty ( [], "Cmd" )) [ Node empty <| Typed (Node empty ( [], "Msg" )) [] ]
                                                    ]
                                            )
                                    )
                            )
                        )
        , test "function with arrow with spacing on indent 0" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "msg -> Cmd model\n\nsomeFunction" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                FunctionTypeAnnotation (Node empty <| GenericType "msg")
                                    (Node empty <| Typed (Node empty ( [], "Cmd" )) [ Node empty <| GenericType "model" ])
                            )
                        )
        , test "function as argument" <|
            \() ->
                parseFullStringWithNullState "( cMsg -> cModel -> a ) -> b" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <| GenericType "cMsg")
                                            (Node empty <|
                                                FunctionTypeAnnotation
                                                    (Node empty <| GenericType "cModel")
                                                    (Node empty <| GenericType "a")
                                            )
                                    )
                                    (Node empty <| GenericType "b")
                            )
                        )
        , test "type with params" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <| Typed (Node empty ( [], "Foo" )) [])
                                    (Node empty <| Typed (Node empty ( [], "Bar" )) [])
                            )
                        )
        , test "function type reference multiple and parens" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar) -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node empty <|
                                FunctionTypeAnnotation
                                    (Node empty <|
                                        FunctionTypeAnnotation
                                            (Node empty <| Typed (Node empty ( [], "Foo" )) [])
                                            (Node empty <| Typed (Node empty ( [], "Bar" )) [])
                                    )
                                    (Node empty <| GenericType "baz")
                            )
                        )
        , test "parseTypeWith wrong indent" <|
            \() ->
                parseFullStringWithNullState "Maybe\na" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal Nothing
        , test "parseTypeWith good indent" <|
            \() ->
                parseFullStringWithNullState "Maybe\n a" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                Typed (Node empty ( [], "Maybe" ))
                                    [ Node empty <| GenericType "a" ]
                            )
                        )
        , test "issue #5 - no spaces between type and generic with parens" <|
            \() ->
                parseFullStringWithNullState "List(String)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                Typed (Node empty ( [], "List" ))
                                    [ Node empty <| Typed (Node empty ( [], "String" )) [] ]
                            )
                        )
        , test "parse type with multiple params" <|
            \() ->
                parseFullStringWithNullState "Dict String Int" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node empty <|
                                Typed (Node empty ( [], "Dict" ))
                                    [ Node empty <| Typed (Node empty ( [], "String" )) []
                                    , Node empty <| Typed (Node empty ( [], "Int" )) []
                                    ]
                            )
                        )
        ]
