module Elm.Parser.TypeAnnotationTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.TypeAnnotation as Parser
import Elm.Syntax.Range exposing (emptyRange)
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
                    |> Expect.equal (Just <| Unit emptyRange)
        , test "unitTypeReference with spaces" <|
            \() ->
                parseFullStringWithNullState "( )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Unit emptyRange)
        , test "tupledTypeReference" <|
            \() ->
                parseFullStringWithNullState "( (), ())" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Tupled [ Unit emptyRange, Unit emptyRange ] emptyRange)
        , test "tupledTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "( () )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Unit emptyRange)
        , test "tupledTypeReference 3" <|
            \() ->
                parseFullStringWithNullState "( Int , Maybe m )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Tupled
                                [ Typed [] "Int" [] emptyRange
                                , Typed [] "Maybe" [ GenericType "m" emptyRange ] emptyRange
                                ]
                                emptyRange
                            )
                        )
        , test "qualified type reference" <|
            \() ->
                parseFullStringWithNullState "Foo.Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Typed [ "Foo" ] "Bar" [] emptyRange))
        , test "typeAnnotationNoFn" <|
            \() ->
                parseFullStringWithNullState "Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Typed [] "Bar" [] emptyRange))
        , test "typedTypeReference 1" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Typed []
                                "Foo"
                                [ Unit emptyRange
                                , GenericType "a" emptyRange
                                , Typed [] "Bar" [] emptyRange
                                ]
                                emptyRange
                        )
        , test "typedTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Typed []
                                "Foo"
                                [ Unit emptyRange
                                , GenericType "a" emptyRange
                                , Typed [] "Bar" [] emptyRange
                                ]
                                emptyRange
                        )
        , test "recordTypeReference empty" <|
            \() ->
                parseFullStringWithNullState "{}" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Record [] emptyRange
                        )
        , test "recordTypeReference one field" <|
            \() ->
                parseFullStringWithNullState "{color: String }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Record [ ( "color", Typed [] "String" [] emptyRange ) ] emptyRange
                        )
        , test "recordTypeReference nested record" <|
            \() ->
                parseFullStringWithNullState "{color: {r : Int, g :Int, b: Int } }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Record
                                [ ( "color"
                                  , Record
                                        [ ( "r", Typed [] "Int" [] emptyRange )
                                        , ( "g", Typed [] "Int" [] emptyRange )
                                        , ( "b", Typed [] "Int" [] emptyRange )
                                        ]
                                        emptyRange
                                  )
                                ]
                                emptyRange
                        )
        , test "recordTypeReference with generic" <|
            \() ->
                parseFullStringWithNullState "{color: s }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Record
                                [ ( "color"
                                  , GenericType "s" emptyRange
                                  )
                                ]
                                emptyRange
                        )
        , test "function type reference" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (Typed [] "Foo" [] emptyRange)
                                (Typed [] "Bar" [] emptyRange)
                                emptyRange
                        )
        , test "function type reference multiple" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (Typed [] "Foo" [] emptyRange)
                                (FunctionTypeAnnotation
                                    (Typed [] "Bar" [] emptyRange)
                                    (GenericType "baz" emptyRange)
                                    emptyRange
                                )
                                emptyRange
                        )
        , test "function type reference generics" <|
            \() ->
                parseFullStringWithNullState "cMsg -> cModel -> a" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (GenericType "cMsg" emptyRange)
                                (FunctionTypeAnnotation
                                    (GenericType "cModel" emptyRange)
                                    (GenericType "a" emptyRange)
                                    emptyRange
                                )
                                emptyRange
                        )
        , test "function as argument" <|
            \() ->
                parseFullStringWithNullState "( cMsg -> cModel -> a ) -> b" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (FunctionTypeAnnotation
                                    (GenericType "cMsg" emptyRange)
                                    (FunctionTypeAnnotation
                                        (GenericType "cModel" emptyRange)
                                        (GenericType "a" emptyRange)
                                        emptyRange
                                    )
                                    emptyRange
                                )
                                (GenericType "b" emptyRange)
                                emptyRange
                        )
        , test "type with params" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (Typed [] "Foo" [] emptyRange)
                                (Typed [] "Bar" [] emptyRange)
                                emptyRange
                        )
        , test "function type reference multiple and parens" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar) -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            FunctionTypeAnnotation
                                (FunctionTypeAnnotation
                                    (Typed [] "Foo" [] emptyRange)
                                    (Typed [] "Bar" [] emptyRange)
                                    emptyRange
                                )
                                (GenericType "baz" emptyRange)
                                emptyRange
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
                    |> Expect.equal (Just (Typed [] "Maybe" [ GenericType "a" emptyRange ] emptyRange))
        ]
