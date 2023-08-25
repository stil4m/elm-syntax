module Elm.Parser.TypingsTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Typings as Parser exposing (TypeDefinition(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


asTypeAlias : TypeDefinition -> Maybe TypeAlias
asTypeAlias td =
    case td of
        DefinedAlias _ t ->
            Just t

        _ ->
            Nothing


all : Test
all =
    describe "TypeAlias"
        [ test "type alias" <|
            \() ->
                parseFullStringWithNullState "type alias Foo = {color: String }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node.empty "Foo"
                            , generics = []
                            , typeAnnotation =
                                Node.empty <|
                                    Record
                                        [ Node.empty <|
                                            ( Node.empty <| "color"
                                            , Node.empty <| Typed (Node.empty <| ( [], "String" )) []
                                            )
                                        ]
                            }
                        )
        , test "type alias without spacings around '='" <|
            \() ->
                parseFullStringWithNullState "type alias Foo={color: String }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node.empty "Foo"
                            , generics = []
                            , typeAnnotation =
                                Node.empty <|
                                    Record
                                        [ Node.empty <|
                                            ( Node.empty <| "color"
                                            , Node.empty <| Typed (Node.empty <| ( [], "String" )) []
                                            )
                                        ]
                            }
                        )
        , test "type alias with GenericType " <|
            \() ->
                parseFullStringWithNullState "type alias Foo a = {some : a }" Parser.typeDefinition
                    |> Maybe.andThen asTypeAlias
                    |> Maybe.map noRangeTypeAlias
                    |> Expect.equal
                        (Just <|
                            { documentation = Nothing
                            , name = Node empty <| "Foo"
                            , generics = [ Node empty <| "a" ]
                            , typeAnnotation =
                                Node empty <|
                                    Record
                                        [ Node empty <|
                                            ( Node empty <| "some"
                                            , Node empty <| GenericType "a"
                                            )
                                        ]
                            }
                        )
        , test "type" <|
            \() ->
                "type Color = Blue String | Red | Green"
                    |> expectAst
                        (DefinedType { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            { constructors =
                                [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                    { name = Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                    , arguments =
                                        [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                            (Typed (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                        ]
                                    }
                                , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                    { name = Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                    , arguments = []
                                    }
                                , Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                    { name = Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                    , arguments = []
                                    }
                                ]
                            , documentation = Nothing
                            , generics = []
                            , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                            }
                        )
        , test "type with multiple args" <|
            \() ->
                "type D = C a B"
                    |> expectAst
                        (DefinedType { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            { constructors =
                                [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                    { arguments =
                                        [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (GenericType "a")
                                        , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                            (Typed (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
                                        ]
                                    , name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                    }
                                ]
                            , documentation = Nothing
                            , generics = []
                            , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                            }
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                "type D = C B a"
                    |> expectAst
                        (DefinedType { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            { constructors =
                                [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                    { arguments =
                                        [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                            (Typed (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                        , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GenericType "a")
                                        ]
                                    , name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                    }
                                ]
                            , documentation = Nothing
                            , generics = []
                            , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                            }
                        )
        , test "type args should not continue on next line" <|
            \() ->
                "type D = C B\na"
                    |> expectInvalid
        , test "type with GenericType" <|
            \() ->
                "type Maybe a = Just a | Nothing"
                    |> expectAst
                        (DefinedType { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            { constructors =
                                [ Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                    { arguments = [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (GenericType "a") ]
                                    , name = Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                    }
                                , Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                    { arguments = []
                                    , name = Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
                                    }
                                ]
                            , documentation = Nothing
                            , generics = [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                            , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                            }
                        )
        , test "type with value on next line " <|
            \() ->
                parseFullStringWithNullState "type Maybe a = Just a |\nNothing" Parser.typeDefinition
                    |> Expect.equal Nothing
        , test "type with spacing after " <|
            \() ->
                "type A = B\n\n"
                    |> expectAst
                        (DefinedType { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            { constructors =
                                [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } }
                                    { arguments = [], name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "B" }
                                ]
                            , documentation = Nothing
                            , generics = []
                            , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "A"
                            }
                        )
        ]


expectAst : TypeDefinition -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source Parser.typeDefinition of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parseFullStringWithNullState source Parser.typeDefinition of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
