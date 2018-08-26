module Elm.Parser.TypeAnnotationTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (emptyState)
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
        , test "unitTypeReference with spaces" <|
            \() ->
                parseFullStringWithNullState "( )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal Nothing
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
                parseFullStringWithNullState "( () , Maybe m )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , Tupled
                                [ ( emptyRange, Unit )
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
        , test "record with generic" <|
            \() ->
                parseFullStringWithNullState "{ attr | position : Vec2, texture : Vec2 }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , GenericRecord "attr"
                                [ ( "position", ( emptyRange, Typed [] "Vec2" [] ) )
                                , ( "texture", ( emptyRange, Typed [] "Vec2" [] ) )
                                ]
                            )
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
        , test "function with spacing on indent 0" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Model\n\nsomeFunction" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just ( emptyRange, Typed [] "Model" [] ))
        , test "annotation with parens" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Msg -> Model -> (Model, Cmd Msg)\n\n" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            ( emptyRange
                            , FunctionTypeAnnotation ( emptyRange, Typed [] "Msg" [] )
                                ( emptyRange
                                , FunctionTypeAnnotation ( emptyRange, Typed [] "Model" [] )
                                    ( emptyRange
                                    , Tupled
                                        [ ( emptyRange, Typed [] "Model" [] )
                                        , ( emptyRange, Typed [] "Cmd" [ ( emptyRange, Typed [] "Msg" [] ) ] )
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
                            ( emptyRange
                            , FunctionTypeAnnotation ( emptyRange, GenericType "msg" )
                                ( emptyRange
                                , Typed [] "Cmd" [ ( emptyRange, GenericType "model" ) ]
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
