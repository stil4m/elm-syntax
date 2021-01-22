module Elm.Parser.ModuleTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as File
import Elm.Parser.Modules as Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ModuleTests"
        [ test "formatted moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "module Foo exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal
                        (Just
                            (NormalModule
                                { moduleName = Node emptyRange <| [ "Foo" ]
                                , exposingList = Node emptyRange <| Explicit (Node emptyRange <| TypeOrAliasExpose "Bar") []
                                }
                            )
                        )
        , test "port moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| Explicit (Node emptyRange <| TypeOrAliasExpose "Bar") [] }))
        , test "port moduleDefinition with spacing" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing ( Bar )" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| Explicit (Node emptyRange <| TypeOrAliasExpose "Bar") [] }))
        , test "effect moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal
                        (Just
                            (EffectModule
                                { moduleName = Node emptyRange <| [ "Foo" ]
                                , exposingList = Node emptyRange <| Explicit (Node emptyRange <| TypeOrAliasExpose "Bar") []
                                , command = Just <| Node emptyRange <| "MyCmd"
                                , subscription = Just <| Node emptyRange <| "MySub"
                                }
                            )
                        )
        , test "unformatted" <|
            \() ->
                parseFullStringWithNullState "module \n Foo \n exposing  (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| All emptyRange }))
        , test "unformatted wrong" <|
            \() ->
                parseFullStringWithNullState "module \nFoo \n exposing  (..)" Parser.moduleDefinition
                    |> Expect.equal Nothing
        , test "exposing all" <|
            \() ->
                parseFullStringWithNullState "module Foo exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "Foo" ], exposingList = Node emptyRange <| All emptyRange }))
        , test "module name with _" <|
            \() ->
                parseFullStringWithNullState "module I_en_gb exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node emptyRange <| [ "I_en_gb" ], exposingList = Node emptyRange <| All emptyRange }))
        , test "Incorrect range in if expression regression test" <|
            \() ->
                parseFullStringWithNullState
                    """module TestModule exposing (..)

a =
    if cond then
        1
    else
        2



{-| doc
-}
b = 3
"""
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = [ Node { end = { column = 3, row = 12 }, start = { column = 1, row = 11 } } "{-| doc\u{000D}\n-}" ]
                            , declarations =
                                [ Node { end = { column = 1, row = 13 }, start = { column = 1, row = 3 } }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { end = { column = 10, row = 7 }, start = { column = 1, row = 3 } }
                                                { arguments = []
                                                , expression =
                                                    Node { end = { column = 10, row = 7 }, start = { column = 5, row = 4 } }
                                                        (IfBlock
                                                            (Node { end = { column = 12, row = 4 }, start = { column = 8, row = 4 } }
                                                                (FunctionOrValue [] "cond")
                                                            )
                                                            (Node { end = { column = 10, row = 5 }, start = { column = 9, row = 5 } } (Integer 1))
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
                                                , name = Node { end = { column = 2, row = 3 }, start = { column = 1, row = 3 } } "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                , Node { end = { column = 6, row = 13 }, start = { column = 1, row = 13 } }
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
                                                { arguments = [], expression = Node { end = { column = 6, row = 13 }, start = { column = 5, row = 13 } } (Integer 3), name = Node { end = { column = 2, row = 13 }, start = { column = 1, row = 13 } } "b" }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Node { end = { column = 32, row = 1 }, start = { column = 1, row = 1 } }
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
                                                (All { end = { column = 31, row = 1 }, start = { column = 29, row = 1 } })
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
                parseFullStringWithNullState
                    """module TestModule exposing (..)

a =
    2



{-| doc
-}
b = 3
"""
                    File.file
                    |> Expect.equal
                        (Just
                            { comments = [ Node { end = { column = 3, row = 9 }, start = { column = 1, row = 8 } } "{-| doc\u{000D}\n-}" ]
                            , declarations =
                                [ Node
                                    { end = { column = 6, row = 4 }
                                    , start = { column = 1, row = 3 }
                                    }
                                    (FunctionDeclaration
                                        { declaration =
                                            Node { end = { column = 6, row = 4 }, start = { column = 1, row = 3 } }
                                                { arguments = []
                                                , expression =
                                                    Node
                                                        { end =
                                                            { column = 6, row = 4 }
                                                        , start = { column = 5, row = 4 }
                                                        }
                                                        (Integer 2)
                                                , name = Node { end = { column = 2, row = 3 }, start = { column = 1, row = 3 } } "a"
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
                                            Node { end = { column = 6, row = 10 }, start = { column = 1, row = 10 } }
                                                { arguments = []
                                                , expression =
                                                    Node
                                                        { end =
                                                            { column = 6, row = 10 }
                                                        , start = { column = 5, row = 10 }
                                                        }
                                                        (Integer 3)
                                                , name = Node { end = { column = 2, row = 10 }, start = { column = 1, row = 10 } } "b"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , imports = []
                            , moduleDefinition =
                                Node { end = { column = 32, row = 1 }, start = { column = 1, row = 1 } }
                                    (NormalModule
                                        { exposingList =
                                            Node { end = { column = 32, row = 1 }, start = { column = 19, row = 1 } }
                                                (All
                                                    { end =
                                                        { column = 31, row = 1 }
                                                    , start = { column = 29, row = 1 }
                                                    }
                                                )
                                        , moduleName = Node { end = { column = 18, row = 1 }, start = { column = 8, row = 1 } } [ "TestModule" ]
                                        }
                                    )
                            }
                        )
        ]
