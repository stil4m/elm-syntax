module Elm.Parser.TypeAnnotationTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.TypeAnnotation as Parser
import Elm.Syntax.Node exposing (Node(..))
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
                    |> Expect.equal (Just <| Node emptyRange (Tuple []))
        , test "unitTypeReference with spaces" <|
            \() ->
                parseFullStringWithNullState "( )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal Nothing
        , test "tupledTypeReference" <|
            \() ->
                parseFullStringWithNullState "( (), ())" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Node emptyRange <| Tuple [ Node emptyRange (Tuple []), Node emptyRange (Tuple []) ])
        , test "tupledTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "( () )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just <| Node emptyRange (Tuple []))
        , test "tupledTypeReference 3" <|
            \() ->
                parseFullStringWithNullState "( () , Maybe m )" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node emptyRange <|
                                Tuple
                                    [ Node emptyRange (Tuple [])
                                    , Node emptyRange <| Type (Node emptyRange <| ( [], "Maybe" )) [ Node emptyRange <| Var "m" ]
                                    ]
                            )
                        )
        , test "qualified type reference" <|
            \() ->
                parseFullStringWithNullState "Foo.Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Node emptyRange <| Type (Node emptyRange ( [ "Foo" ], "Bar" )) []))
        , test "typeAnnotationNoFn" <|
            \() ->
                parseFullStringWithNullState "Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal (Just (Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) []))
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
                            (Node emptyRange <|
                                Type (Node emptyRange ( [], "Foo" ))
                                    [ Node emptyRange (Tuple [])
                                    , Node emptyRange <| Var "a"
                                    , Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) []
                                    ]
                            )
                        )
        , test "typedTypeReference 2" <|
            \() ->
                parseFullStringWithNullState "Foo () a Bar" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                Type (Node emptyRange ( [], "Foo" ))
                                    [ Node emptyRange (Tuple [])
                                    , Node emptyRange <| Var "a"
                                    , Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) []
                                    ]
                            )
                        )
        , test "recordTypeReference empty" <|
            \() ->
                parseFullStringWithNullState "{}" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Node emptyRange <|
                                Record []
                        )
        , test "recordTypeReference one field" <|
            \() ->
                parseFullStringWithNullState "{color: String }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            Node emptyRange <|
                                Record
                                    [ Node emptyRange ( Node emptyRange "color", Node emptyRange <| Type (Node emptyRange ( [], "String" )) [] ) ]
                        )
        , test "record with generic" <|
            \() ->
                parseFullStringWithNullState "{ attr | position : Vec2, texture : Vec2 }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node emptyRange <|
                                ExtensionRecord
                                    (Node emptyRange "attr")
                                    (Node emptyRange ( Node emptyRange "position", Node emptyRange <| Type (Node emptyRange <| ( [], "Vec2" )) [] ))
                                    [ Node emptyRange ( Node emptyRange "texture", Node emptyRange <| Type (Node emptyRange ( [], "Vec2" )) [] ) ]
                            )
                        )
        , test "recordTypeReference nested record" <|
            \() ->
                parseFullStringWithNullState "{color: {r : Int, g :Int, b: Int } }" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                Record
                                    [ Node emptyRange
                                        ( Node emptyRange "color"
                                        , Node emptyRange <|
                                            Record
                                                [ Node emptyRange ( Node emptyRange "r", Node emptyRange <| Type (Node emptyRange ( [], "Int" )) [] )
                                                , Node emptyRange ( Node emptyRange "g", Node emptyRange <| Type (Node emptyRange ( [], "Int" )) [] )
                                                , Node emptyRange ( Node emptyRange "b", Node emptyRange <| Type (Node emptyRange ( [], "Int" )) [] )
                                                ]
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
                            (Node emptyRange <|
                                Record
                                    [ Node emptyRange
                                        ( Node emptyRange "color"
                                        , Node emptyRange <| Var "s"
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
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Foo" )) [])
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) [])
                            )
                        )
        , test "function type reference multiple" <|
            \() ->
                parseFullStringWithNullState "Foo -> Bar -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Foo" )) [])
                                    (Node emptyRange <|
                                        FunctionTypeAnnotation
                                            (Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) [])
                                            (Node emptyRange <| Var "baz")
                                    )
                            )
                        )
        , test "function type reference generics" <|
            \() ->
                parseFullStringWithNullState "cMsg -> cModel -> a" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <| Var "cMsg")
                                    (Node emptyRange <|
                                        FunctionTypeAnnotation
                                            (Node emptyRange <| Var "cModel")
                                            (Node emptyRange <| Var "a")
                                    )
                            )
                        )
        , test "function with spacing on indent 0" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Model\n\nsomeFunction" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just (Node emptyRange <| Type (Node emptyRange ( [], "Model" )) []))
        , test "annotation with parens" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "Msg -> Model -> (Model, Cmd Msg)\n\n" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node emptyRange <|
                                FunctionTypeAnnotation (Node emptyRange <| Type (Node emptyRange ( [], "Msg" )) [])
                                    (Node emptyRange <|
                                        FunctionTypeAnnotation (Node emptyRange <| Type (Node emptyRange ( [], "Model" )) [])
                                            (Node emptyRange <|
                                                Tuple
                                                    [ Node emptyRange <| Type (Node emptyRange ( [], "Model" )) []
                                                    , Node emptyRange <| Type (Node emptyRange ( [], "Cmd" )) [ Node emptyRange <| Type (Node emptyRange ( [], "Msg" )) [] ]
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
                            (Node emptyRange <|
                                FunctionTypeAnnotation (Node emptyRange <| Var "msg")
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Cmd" )) [ Node emptyRange <| Var "model" ])
                            )
                        )
        , test "function as argument" <|
            \() ->
                parseFullStringWithNullState "( cMsg -> cModel -> a ) -> b" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <|
                                        FunctionTypeAnnotation
                                            (Node emptyRange <| Var "cMsg")
                                            (Node emptyRange <|
                                                FunctionTypeAnnotation
                                                    (Node emptyRange <| Var "cModel")
                                                    (Node emptyRange <| Var "a")
                                            )
                                    )
                                    (Node emptyRange <| Var "b")
                            )
                        )
        , test "type with params" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Foo" )) [])
                                    (Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) [])
                            )
                        )
        , test "function type reference multiple and parens" <|
            \() ->
                parseFullStringWithNullState "(Foo -> Bar) -> baz" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just <|
                            (Node emptyRange <|
                                FunctionTypeAnnotation
                                    (Node emptyRange <|
                                        FunctionTypeAnnotation
                                            (Node emptyRange <| Type (Node emptyRange ( [], "Foo" )) [])
                                            (Node emptyRange <| Type (Node emptyRange ( [], "Bar" )) [])
                                    )
                                    (Node emptyRange <| Var "baz")
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
                            (Node emptyRange <|
                                Type (Node emptyRange ( [], "Maybe" ))
                                    [ Node emptyRange <| Var "a" ]
                            )
                        )
        , test "issue #5 - no spaces between type and generic with parens" <|
            \() ->
                parseFullStringWithNullState "List(String)" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node emptyRange <|
                                Type (Node emptyRange ( [], "List" ))
                                    [ Node emptyRange <| Type (Node emptyRange ( [], "String" )) [] ]
                            )
                        )
        , test "parse type with multiple params" <|
            \() ->
                parseFullStringWithNullState "Dict String Int" Parser.typeAnnotation
                    |> Maybe.map noRangeTypeReference
                    |> Expect.equal
                        (Just
                            (Node emptyRange <|
                                Type (Node emptyRange ( [], "Dict" ))
                                    [ Node emptyRange <| Type (Node emptyRange ( [], "String" )) []
                                    , Node emptyRange <| Type (Node emptyRange ( [], "Int" )) []
                                    ]
                            )
                        )
        ]
