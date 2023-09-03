module Elm.Parser.ModuleTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil exposing (..)
import Elm.Parser.File as File
import Elm.Parser.Modules as Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "ModuleTests"
        [ test "formatted moduleDefinition" <|
            \() ->
                "module Foo exposing (Bar)"
                    |> expectAst
                        (NormalModule
                            { exposingList =
                                Node { start = { row = 1, column = 12 }, end = { row = 1, column = 26 } }
                                    (Explicit
                                        [ Node { start = { row = 1, column = 22 }, end = { row = 1, column = 25 } } (TypeOrAliasExpose "Bar")
                                        ]
                                    )
                            , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            }
                        )
        , test "port moduleDefinition" <|
            \() ->
                "port module Foo exposing (Bar)"
                    |> expectAst
                        (PortModule
                            { exposingList =
                                Node { start = { row = 1, column = 17 }, end = { row = 1, column = 31 } }
                                    (Explicit
                                        [ Node { start = { row = 1, column = 27 }, end = { row = 1, column = 30 } } (TypeOrAliasExpose "Bar") ]
                                    )
                            , moduleName = Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                            }
                        )
        , test "port moduleDefinition with spacing" <|
            \() ->
                "port module Foo exposing ( Bar )"
                    |> expectAst
                        (PortModule
                            { exposingList =
                                Node { start = { row = 1, column = 17 }, end = { row = 1, column = 33 } }
                                    (Explicit
                                        [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (TypeOrAliasExpose "Bar") ]
                                    )
                            , moduleName = Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } [ "Foo" ]
                            }
                        )
        , test "effect moduleDefinition" <|
            \() ->
                "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)"
                    |> expectAst
                        (EffectModule
                            { moduleName = Node { start = { row = 1, column = 15 }, end = { row = 1, column = 18 } } [ "Foo" ]
                            , command = Just (Node { start = { row = 1, column = 36 }, end = { row = 1, column = 41 } } "MyCmd")
                            , subscription = Just (Node { start = { row = 1, column = 58 }, end = { row = 1, column = 63 } } "MySub")
                            , exposingList = Node { start = { row = 1, column = 66 }, end = { row = 1, column = 80 } } (Explicit [ Node { start = { row = 1, column = 76 }, end = { row = 1, column = 79 } } (TypeOrAliasExpose "Bar") ])
                            }
                        )
        , test "unformatted" <|
            \() ->
                "module \n Foo \n exposing  (..)"
                    |> expectAst
                        (NormalModule
                            { exposingList =
                                Node { start = { row = 3, column = 2 }, end = { row = 3, column = 16 } }
                                    (All { start = { row = 3, column = 13 }, end = { row = 3, column = 15 } })
                            , moduleName = Node { start = { row = 2, column = 2 }, end = { row = 2, column = 5 } } [ "Foo" ]
                            }
                        )
        , test "unformatted wrong" <|
            \() ->
                parse "module \nFoo \n exposing  (..)" Parser.moduleDefinition
                    |> Expect.equal Nothing
        , test "exposing all" <|
            \() ->
                "module Foo exposing (..)"
                    |> expectAst
                        (NormalModule
                            { exposingList =
                                Node { start = { row = 1, column = 12 }, end = { row = 1, column = 25 } }
                                    (All { start = { row = 1, column = 22 }, end = { row = 1, column = 24 } })
                            , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } [ "Foo" ]
                            }
                        )
        , test "module name with _" <|
            \() ->
                "module I_en_gb exposing (..)"
                    |> expectAst
                        (NormalModule
                            { exposingList =
                                Node { start = { row = 1, column = 16 }, end = { row = 1, column = 29 } }
                                    (All { start = { row = 1, column = 26 }, end = { row = 1, column = 28 } })
                            , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } [ "I_en_gb" ]
                            }
                        )
        , test "Regression test for Incorrect range in if expression" <|
            \() ->
                parse
                    (String.filter ((/=) '\u{000D}') """module TestModule exposing (..)

a =
    if cond then
        1
    else
        2



{-| doc
-}
b = 3
""")
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 11, column = 1 }, end = { row = 12, column = 3 } } "{-| doc\n-}" ]
                            , declarations =
                                [ Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 3, column = 1 }, end = { row = 7, column = 10 } }
                                                { arguments = []
                                                , expression =
                                                    Node { start = { row = 4, column = 5 }, end = { row = 7, column = 10 } }
                                                        (IfBlock
                                                            (Node { start = { row = 4, column = 8 }, end = { row = 4, column = 12 } }
                                                                (FunctionOrValue [] "cond")
                                                            )
                                                            (Node { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } (Integer 1))
                                                            (Node
                                                                { end = { column = 10, row = 7 }
                                                                , start =
                                                                    { column = 9
                                                                    , row = 7
                                                                    }
                                                                }
                                                                (Integer 2)
                                                            )
                                                        )
                                                , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                , Node { start = { row = 13, column = 1 }, end = { row = 13, column = 6 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node
                                                { end =
                                                    { column = 6
                                                    , row =
                                                        13
                                                    }
                                                , start = { column = 1, row = 13 }
                                                }
                                                { arguments = []
                                                , expression = Node { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } } (Integer 3)
                                                , name = Node { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } } "b"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    (NormalModule
                                        { exposingList =
                                            Node
                                                { end =
                                                    { column =
                                                        32
                                                    , row = 1
                                                    }
                                                , start = { column = 19, row = 1 }
                                                }
                                                (All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                        , moduleName =
                                            Node
                                                { end =
                                                    { column = 18, row = 1 }
                                                , start = { column = 8, row = 1 }
                                                }
                                                [ "TestModule" ]
                                        }
                                    )
                            }
                        )
        , test "Simple module range test" <|
            \() ->
                parse
                    (String.filter ((/=) '\u{000D}') """module TestModule exposing (..)

a =
    2



{-| doc
-}
b = 3
""")
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = [ Node { start = { row = 8, column = 1 }, end = { row = 9, column = 3 } } "{-| doc\n-}" ]
                            , declarations =
                                [ Node
                                    { end = { column = 6, row = 4 }
                                    , start = { column = 1, row = 3 }
                                    }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 3, column = 1 }, end = { row = 4, column = 6 } }
                                                { arguments = []
                                                , expression =
                                                    Node
                                                        { end =
                                                            { column = 6, row = 4 }
                                                        , start = { column = 5, row = 4 }
                                                        }
                                                        (Integer 2)
                                                , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                , Node
                                    { end =
                                        { column = 6, row = 10 }
                                    , start = { column = 1, row = 10 }
                                    }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 10, column = 1 }, end = { row = 10, column = 6 } }
                                                { arguments = []
                                                , expression =
                                                    Node
                                                        { end =
                                                            { column = 6, row = 10 }
                                                        , start = { column = 5, row = 10 }
                                                        }
                                                        (Integer 3)
                                                , name = Node { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } } "b"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                (All
                                                    { end =
                                                        { column = 31, row = 1 }
                                                    , start = { column = 29, row = 1 }
                                                    }
                                                )
                                        , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                        }
                                    )
                            }
                        )
        , test "File with multiple imports" <|
            \() ->
                parse
                    """module TestModule exposing (..)
import A
import B

a = 1
"""
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = []
                            , moduleDefinition =
                                Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                (All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                        , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                        }
                                    )
                            , imports =
                                [ Node { start = { row = 2, column = 1 }, end = { row = 2, column = 9 } }
                                    { exposingList = Nothing
                                    , moduleAlias = Nothing
                                    , moduleName = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } } [ "A" ]
                                    }
                                , Node { start = { row = 3, column = 1 }, end = { row = 3, column = 9 } }
                                    { exposingList = Nothing
                                    , moduleAlias = Nothing
                                    , moduleName = Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } [ "B" ]
                                    }
                                ]
                            , declarations =
                                [ Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 5, column = 1 }, end = { row = 5, column = 6 } }
                                                { arguments = []
                                                , expression = Node { start = { row = 5, column = 5 }, end = { row = 5, column = 6 } } (Integer 1)
                                                , name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            }
                        )
        , test "File with multiple declarations" <|
            \() ->
                parse
                    """module TestModule exposing (..)
type A = B | C
a = 1
type alias B = A
b : Int
b = 2
"""
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = []
                            , moduleDefinition =
                                Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                                    (NormalModule
                                        { exposingList = Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } } (All { start = { row = 1, column = 29 }, end = { row = 1, column = 31 } })
                                        , moduleName = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 18 } } [ "TestModule" ]
                                        }
                                    )
                            , imports = []
                            , declarations =
                                [ Node { start = { row = 2, column = 1 }, end = { row = 2, column = 15 } }
                                    (CustomTypeDeclaration
                                        { constructors =
                                            [ Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } }
                                                { arguments = []
                                                , name = Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } "B"
                                                }
                                            , Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } }
                                                { arguments = []
                                                , name = Node { start = { row = 2, column = 14 }, end = { row = 2, column = 15 } } "C"
                                                }
                                            ]
                                        , documentation = Nothing
                                        , generics = []
                                        , name = Node { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } } "A"
                                        }
                                    )
                                , Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { start = { row = 3, column = 1 }, end = { row = 3, column = 6 } }
                                                { arguments = []
                                                , expression = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } (Integer 1)
                                                , name = Node { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } } "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                , Node { start = { row = 4, column = 1 }, end = { row = 4, column = 17 } }
                                    (AliasDeclaration
                                        { documentation = Nothing
                                        , generics = []
                                        , name = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } "B"
                                        , typeAnnotation =
                                            Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } }
                                                (Typed (Node { start = { row = 4, column = 16 }, end = { row = 4, column = 17 } } ( [], "A" )) [])
                                        }
                                    )
                                , Node { start = { row = 5, column = 1 }, end = { row = 6, column = 6 } }
                                    (FunctionDeclaration
                                        { documentation = Nothing
                                        , signature =
                                            Just
                                                (Node { start = { row = 5, column = 1 }, end = { row = 5, column = 8 } }
                                                    { name = Node { start = { row = 5, column = 1 }, end = { row = 5, column = 2 } } "b"
                                                    , typeAnnotation = Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } (Typed (Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } ( [], "Int" )) [])
                                                    }
                                                )
                                        , declaration =
                                            Node { start = { row = 6, column = 1 }, end = { row = 6, column = 6 } }
                                                { arguments = []
                                                , expression = Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } (Integer 2)
                                                , name = Node { start = { row = 6, column = 1 }, end = { row = 6, column = 2 } } "b"
                                                }
                                        }
                                    )
                                ]
                            }
                        )
        , test "should fail to parse two signatures in a row" <|
            \() ->
                """module TestModule exposing (..)
a : Int
b : Int
b = 2
"""
                    |> expectInvalid
        , test "should fail to parse signature for a different function" <|
            \() ->
                """module TestModule exposing (..)
a : Int
b = 2
"""
                    |> expectInvalid
        ]


expectAst : Module -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst Parser.moduleDefinition


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid File.file
