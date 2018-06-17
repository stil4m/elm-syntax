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
                    |> Expect.equal (Just <| ( emptyRange, Unit ))
        , test "tupledTypeReference" <|
            \() ->
                parseFullStringWithNullState "( (), ())" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| ( emptyRange, Tupled [ ( emptyRange, Unit ), ( emptyRange, Unit ) ] ))
        , test "tupledTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "( () )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| ( emptyRange, Unit ))
        , test "tupledTypeReference 3" <|
            \() ->
                parseFullStringWithNullState "( Int , Maybe m )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , Tupled
                                [ ( emptyRange, Typed [] "Int" [] )
                                , ( emptyRange, Typed [] "Maybe" [ ( emptyRange, GenericType "m" ) ] )
                                ]
                            )
                        )
        , test "qualified type reference" <|
            \() ->
                parseFullStringWithNullState "Foo.Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just ( emptyRange, Typed [ "Foo" ] "Bar" [] ))
        , test "typeAnnotationNoFn" <|
            \() ->
                parseFullStringWithNullState "Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just ( emptyRange, Typed [] "Bar" [] ))
        , test "typedTypeReference 1" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , Typed []
                                "Foo"
                                [ ( emptyRange, Unit )
                                , ( emptyRange, GenericType "a" )
                                , ( emptyRange, Typed [] "Bar" [] )
                                ]
                            )
                        )
        , test "typedTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , Typed []
                                "Foo"
                                [ ( emptyRange, Unit )
                                , ( emptyRange, GenericType "a" )
                                , ( emptyRange, Typed [] "Bar" [] )
                                ]
                            )
                        )
        , test "recordTypeReference empty" <|
            \() ->
                parseFullStringWithNullState "{}" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange, Record [] )
                        )
        , test "recordTypeReference one field" <|
            \() ->
                parseFullStringWithNullState "{color: String }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange, Record [ ( "color", ( emptyRange, Typed [] "String" [] ) ) ] )
                        )
        , test "recordTypeReference nested record" <|
            \() ->
                parseFullStringWithNullState "{color: {r : Int, g :Int, b: Int } }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , Record
                                [ ( "color"
                                  , ( emptyRange
                                    , Record
                                        [ ( "r", ( emptyRange, Typed [] "Int" [] ) )
                                        , ( "g", ( emptyRange, Typed [] "Int" [] ) )
                                        , ( "b", ( emptyRange, Typed [] "Int" [] ) )
                                        ]
                                    )
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
                            ( emptyRange
                            , Record
                                [ ( "color"
                                  , ( emptyRange, GenericType "s" )
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
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange, Typed [] "Foo" [] )
                                ( emptyRange, Typed [] "Bar" [] )
                            )
                        )
        , test "function type reference multiple" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange, Typed [] "Foo" [] )
                                ( emptyRange
                                , FunctionTypeAnnotation
                                    ( emptyRange, Typed [] "Bar" [] )
                                    ( emptyRange, GenericType "baz" )
                                )
                            )
                        )
        , test "function type reference generics" <|
            \() ->
                parseFullStringWithNullState "cMsg -> cModel -> a" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange, GenericType "cMsg" )
                                ( emptyRange
                                , FunctionTypeAnnotation
                                    ( emptyRange, GenericType "cModel" )
                                    ( emptyRange, GenericType "a" )
                                )
                            )
                        )
        , test "function as argument" <|
            \() ->
                parseFullStringWithNullState "( cMsg -> cModel -> a ) -> b" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange
                                , FunctionTypeAnnotation
                                    ( emptyRange, GenericType "cMsg" )
                                    ( emptyRange
                                    , FunctionTypeAnnotation
                                        ( emptyRange, GenericType "cModel" )
                                        ( emptyRange, GenericType "a" )
                                    )
                                )
                                ( emptyRange, GenericType "b" )
                            )
                        )
        , test "type with params" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange, Typed [] "Foo" [] )
                                ( emptyRange, Typed [] "Bar" [] )
                            )
                        )
        , test "function type reference multiple and parens" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar) -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            ( emptyRange
                            , FunctionTypeAnnotation
                                ( emptyRange
                                , FunctionTypeAnnotation
                                    ( emptyRange, Typed [] "Foo" [] )
                                    ( emptyRange, Typed [] "Bar" [] )
                                )
                                ( emptyRange, GenericType "baz" )
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
                    |> Expect.equal (Just ( emptyRange, Typed [] "Maybe" [ ( emptyRange, GenericType "a" ) ] ))
        , test "issue #5 - no spaces between type and generic with parens" <|
            \() ->
                parseFullStringWithNullState "List(String)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just ( emptyRange, Typed [] "List" [ ( emptyRange, Typed [] "String" [] ) ] ))
        , test "parse type with multiple params" <|
            \() ->
                parseFullStringWithNullState "Dict String Int" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , Typed []
                                "Dict"
                                [ ( emptyRange, Typed [] "String" [] )
                                , ( emptyRange, Typed [] "Int" [] )
                                ]
                            )
                        )
        ]
