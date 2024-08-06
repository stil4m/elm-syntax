module Elm.Parser.TypeAnnotationTests exposing (all)

import Elm.Parser.Layout as Layout
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil exposing (..)
import Elm.Parser.TypeAnnotation as Parser
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import ParserFast
import Test exposing (..)


all : Test
all =
    describe "TypeReferenceTests"
        [ test "unitTypeReference" <|
            \() ->
                "()"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } Unit)
        , test "unitTypeReference with spaces" <|
            \() ->
                "( )"
                    |> expectInvalid
        , test "tupledTypeReference" <|
            \() ->
                "( (), ())"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Tupled
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Unit
                                , Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } Unit
                                ]
                            )
                        )
        , test "tupledTypeReference 2" <|
            \() ->
                -- TODO This feels incorrect, there should be a Parenthesized type for this
                "( () )"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } Unit)
        , test "tupledTypeReference 3" <|
            \() ->
                "( () , Maybe m )"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            (Tupled
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 5 } } Unit
                                , Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } }
                                    (Typed
                                        (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Maybe" ))
                                        [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GenericType "m") ]
                                    )
                                ]
                            )
                        )
        , test "qualified type reference" <|
            \() ->
                "Foo.Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } ( [ "Foo" ], "Bar" )) [])
                        )
        , test "typeAnnotationNoFn" <|
            \() ->
                "Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Bar" )) [])
                        )
        , test "types with and without spacing should parse to the same" <|
            \() ->
                parse "Bar " (Parser.typeAnnotation |> ParserFast.ignore Layout.maybeLayout)
                    |> Expect.equal (parse "Bar" Parser.typeAnnotation)
        , test "typedTypeReference 1" <|
            \() ->
                "Foo () a Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Unit
                                , Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (GenericType "a")
                                , Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                    (Typed (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                ]
                            )
                        )
        , test "typedTypeReference 2" <|
            \() ->
                "Foo () a Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" ))
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } Unit
                                , Node { start = { row = 1, column = 8 }, end = { row = 1, column = 9 } } (GenericType "a")
                                , Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } }
                                    (Typed (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 13 } } ( [], "Bar" )) [])
                                ]
                            )
                        )
        , test "recordTypeReference empty" <|
            \() ->
                "{}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (Record []))
        , test "recordTypeReference one field" <|
            \() ->
                "{color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 17 } }
                            (Record
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } }
                                    ( Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } }
                                        (Typed (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } ( [], "String" )) [])
                                    )
                                ]
                            )
                        )
        , test "record with generic" <|
            \() ->
                "{ attr | position : Vec2, texture : Vec2 }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 43 } }
                            (GenericRecord (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } "attr")
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
        , test "generic record with no fields" <|
            \() ->
                "{ attr |}"
                    |> expectInvalid
        , test "recordTypeReference nested record" <|
            \() ->
                "{color: {r : Int, g :Int, b: Int } }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 37 } }
                            (Record
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 35 } }
                                    ( Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 35 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 17 } }
                                                ( Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "r"
                                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } }
                                                    (Typed (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } ( [], "Int" )) [])
                                                )
                                            , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                ( Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } "g"
                                                , Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } }
                                                    (Typed (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } ( [], "Int" )) [])
                                                )
                                            , Node { start = { row = 1, column = 27 }, end = { row = 1, column = 34 } }
                                                ( Node { start = { row = 1, column = 27 }, end = { row = 1, column = 28 } } "b"
                                                , Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } }
                                                    (Typed (Node { start = { row = 1, column = 30 }, end = { row = 1, column = 33 } } ( [], "Int" )) [])
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
        , test "record field ranges" <|
            \() ->
                "{ foo : Int, bar : Int, baz : Int }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } } <|
                            Record
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } <|
                                        Typed (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Int" )) []
                                    )
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 23 } }
                                    ( Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } "bar"
                                    , Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } <|
                                        Typed (Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } ( [], "Int" )) []
                                    )
                                , Node { start = { row = 1, column = 25 }, end = { row = 1, column = 35 } }
                                    ( Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } "baz"
                                    , Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } <|
                                        Typed (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 34 } } ( [], "Int" )) []
                                    )
                                ]
                        )
        , test "recordTypeReference with generic" <|
            \() ->
                "{color: s }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (Record
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 10 } }
                                    ( Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "color"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (GenericType "s")
                                    )
                                ]
                            )
                        )
        , test "function type reference" <|
            \() ->
                "Foo -> Bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                )
                                (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                    (Typed (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                )
                            )
                        )
        , test "function type reference multiple" <|
            \() ->
                "Foo -> Bar -> baz"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Foo" )) [])
                                )
                                (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } }
                                            (Typed (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } ( [], "Bar" )) [])
                                        )
                                        (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } (GenericType "baz"))
                                    )
                                )
                            )
                        )
        , test "function type reference generics" <|
            \() ->
                "cMsg -> cModel -> a"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (GenericType "cMsg"))
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 20 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 15 } } (GenericType "cModel"))
                                        (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (GenericType "a"))
                                    )
                                )
                            )
                        )
        , test "annotation with parens" <|
            \() ->
                "Msg -> Model -> (Model, Cmd Msg)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                    (Typed (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } ( [], "Msg" )) [])
                                )
                                (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 33 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } }
                                            (Typed (Node { start = { row = 1, column = 8 }, end = { row = 1, column = 13 } } ( [], "Model" )) [])
                                        )
                                        (Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                            (Tupled
                                                [ Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } }
                                                    (Typed (Node { start = { row = 1, column = 18 }, end = { row = 1, column = 23 } } ( [], "Model" )) [])
                                                , Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                                    (Typed (Node { start = { row = 1, column = 25 }, end = { row = 1, column = 28 } } ( [], "Cmd" ))
                                                        [ Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } }
                                                            (Typed (Node { start = { row = 1, column = 29 }, end = { row = 1, column = 32 } } ( [], "Msg" )) [])
                                                        ]
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
                        )
        , test "function as argument" <|
            \() ->
                "( cMsg -> cModel -> a ) -> b"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 7 } } (GenericType "cMsg"))
                                        (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 22 } }
                                            (FunctionTypeAnnotation (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } (GenericType "cModel"))
                                                (Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (GenericType "a"))
                                            )
                                        )
                                    )
                                )
                                (Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (GenericType "b"))
                            )
                        )
        , test "type with params" <|
            \() ->
                "(Foo -> Bar)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                    (Typed
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                        []
                                    )
                                )
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                    (Typed
                                        (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                        []
                                    )
                                )
                            )
                        )
        , test "function type reference multiple and parens" <|
            \() ->
                "(Foo -> Bar) -> baz"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } }
                            (FunctionTypeAnnotation
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                                    (FunctionTypeAnnotation
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                            (Typed
                                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } ( [], "Foo" ))
                                                []
                                            )
                                        )
                                        (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } }
                                            (Typed
                                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } ( [], "Bar" ))
                                                []
                                            )
                                        )
                                    )
                                )
                                (Node { start = { row = 1, column = 17 }, end = { row = 1, column = 20 } } (GenericType "baz"))
                            )
                        )
        , test "parseTypeWith wrong indent" <|
            \() ->
                "Maybe\na"
                    |> expectInvalid
        , test "parseTypeWith good indent" <|
            \() ->
                "Maybe\n a"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 3 } }
                            (Typed
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } ( [], "Maybe" ))
                                [ Node { start = { row = 2, column = 2 }, end = { row = 2, column = 3 } } (GenericType "a") ]
                            )
                        )
        , test "issue #5 - no spaces between type and generic with parens" <|
            \() ->
                "List(String)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (Typed
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "List" ))
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } }
                                    (Typed
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                        []
                                    )
                                ]
                            )
                        )
        , test "parse type with multiple params" <|
            \() ->
                "Dict String Int"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Typed
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } ( [], "Dict" ))
                                [ Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }
                                    (Typed
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } ( [], "String" ))
                                        []
                                    )
                                , Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } }
                                    (Typed (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } ( [], "Int" )) [])
                                ]
                            )
                        )
        ]


expectAst : Node TypeAnnotation -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst Parser.typeAnnotation


expectInvalid : String -> Expect.Expectation
expectInvalid =
    ParserWithCommentsUtil.expectInvalid Parser.typeAnnotation
