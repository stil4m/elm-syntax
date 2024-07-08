module Elm.Parser.TypingsTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil exposing (..)
import Elm.Parser.Typings as Parser
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "TypeAlias"
        [ test "type alias" <|
            \() ->
                "type alias Foo = {color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , generics = []
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                ( Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } "color"
                                                , Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                    (Typed (Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias with documentation" <|
            \() ->
                """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                            (AliasDeclaration
                                { documentation = Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } } "{-| Foo is colorful -}")
                                , generics = []
                                , name = Node { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } } "Foo"
                                , typeAnnotation =
                                    Node { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                        (Record
                                            [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                ( Node { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } } "color"
                                                , Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                    (Typed (Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias without spacings around '='" <|
            \() ->
                "type alias Foo={color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , generics = []
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                ( Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } "color"
                                                , Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                    (Typed (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias with GenericType " <|
            \() ->
                "type alias Foo a = {some : a }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , generics = [ Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                ( Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } "some"
                                                , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (GenericType "a")
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type" <|
            \() ->
                "type Color = Blue String | Red | Green"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (Declaration.CustomTypeDeclaration
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
                        )
        , test "type with documentation" <|
            \() ->
                """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                            (Declaration.CustomTypeDeclaration
                                { constructors =
                                    [ Node { start = { row = 2, column = 14 }, end = { row = 2, column = 25 } }
                                        { name = Node { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } } "Blue"
                                        , arguments =
                                            [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                (Typed (Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } } ( [], "String" )) [])
                                            ]
                                        }
                                    , Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                        { name = Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } } "Red"
                                        , arguments = []
                                        }
                                    , Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                        { name = Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } } "Green"
                                        , arguments = []
                                        }
                                    ]
                                , documentation = Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| Classic RGB -}")
                                , generics = []
                                , name = Node { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } } "Color"
                                }
                            )
                        )
        , test "type with multiple args" <|
            \() ->
                "type D = C a B"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
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
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                "type D = C B a"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
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
                        )
        , test "type args should not continue on next line" <|
            \() ->
                "type D = C B\na"
                    |> expectInvalid
        , test "type with GenericType" <|
            \() ->
                "type Maybe a = Just a | Nothing"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (Declaration.CustomTypeDeclaration
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
                        )
        , test "type with value on next line " <|
            \() ->
                parse "type Maybe a = Just a |\nNothing" Parser.typeDefinition
                    |> Expect.equal Nothing
        , test "type with spacing after " <|
            \() ->
                "type A = B\n\n"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Declaration.CustomTypeDeclaration
                                { constructors =
                                    [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } }
                                        { arguments = [], name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "B" }
                                    ]
                                , documentation = Nothing
                                , generics = []
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "A"
                                }
                            )
                        )
        ]


expectAst : Node Declaration -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst Parser.typeDefinition


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid Parser.typeDefinition
