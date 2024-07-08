module Elm.Parser.TypingsTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil as CombineTestUtil exposing (..)
import Elm.Parser.State exposing (State)
import Elm.Parser.Typings as Parser
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
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
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
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
                "type alias Foo = {color: String }"
                    |> expectAstWithDocs (Node { start = { row = -1, column = -1 }, end = { row = 0, column = 0 } } "{-| Foo is colorful -}")
                        (Node { start = { row = -1, column = -1 }, end = { row = 1, column = 34 } }
                            (AliasDeclaration
                                { documentation = Just (Node { start = { row = -1, column = -1 }, end = { row = 0, column = 0 } } "{-| Foo is colorful -}")
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
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
        , test "type alias without spacings around '='" <|
            \() ->
                "type alias Foo={color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
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
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = [ Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
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
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                , generics = []
                                , constructors =
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
                                }
                            )
                        )
        , test "type with documentation" <|
            \() ->
                "type Color = Blue String | Red | Green"
                    |> expectAstWithDocs (Node { start = { row = -1, column = -1 }, end = { row = 0, column = 0 } } "{-| Classic RGB -}")
                        (Node { start = { row = -1, column = -1 }, end = { row = 1, column = 39 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Just (Node { start = { row = -1, column = -1 }, end = { row = 0, column = 0 } } "{-| Classic RGB -}")
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                , generics = []
                                , constructors =
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
                                }
                            )
                        )
        , test "type with multiple args" <|
            \() ->
                "type D = C a B"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , constructors =
                                    [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (GenericType "a")
                                            , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                (Typed (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
                                            ]
                                        }
                                    ]
                                }
                            )
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                "type D = C B a"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , constructors =
                                    [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                                (Typed (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                            , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (GenericType "a")
                                            ]
                                        }
                                    ]
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
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                                , generics = [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                                , constructors =
                                    [ Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                        { name = Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                        , arguments = [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (GenericType "a") ]
                                        }
                                    , Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                        { name = Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
        , test "type with value on next line " <|
            \() ->
                parse "type Maybe a = Just a |\nNothing" typeDefinitionWithoutDocumentation
                    |> Expect.equal Nothing
        , test "type with spacing after " <|
            \() ->
                "type A = B\n\n"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "A"
                                , generics = []
                                , constructors =
                                    [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } }
                                        { name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "B", arguments = [] }
                                    ]
                                }
                            )
                        )
        ]


expectAst : Node Declaration -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst typeDefinitionWithoutDocumentation


expectAstWithDocs : Node Documentation -> Node Declaration -> String -> Expect.Expectation
expectAstWithDocs documentation =
    CombineTestUtil.expectAst (Parser.typeDefinitionAfterDocumentation documentation)


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid typeDefinitionWithoutDocumentation


typeDefinitionWithoutDocumentation : Combine.Parser State (Node Declaration)
typeDefinitionWithoutDocumentation =
    Combine.oneOf
        [ Parser.typeAliasDefinitionWithoutDocumentationWithBacktrackableTypePrefix
        , Parser.customTypeDefinitionWithoutDocumentation
        ]
