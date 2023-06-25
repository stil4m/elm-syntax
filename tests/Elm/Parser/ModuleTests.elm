module Elm.Parser.ModuleTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.File as File
import Elm.Parser.Modules as Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Module exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (empty)
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
                                { moduleName = Node.empty [ "Foo" ]
                                , exposingList = Node.empty <| Explicit [ Node.empty <| TypeOrAliasExpose "Bar" ]
                                }
                            )
                        )
        , test "port moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node.empty <| [ "Foo" ], exposingList = Node.empty <| Explicit [ Node.empty <| TypeOrAliasExpose "Bar" ] }))
        , test "port moduleDefinition with spacing" <|
            \() ->
                parseFullStringWithNullState "port module Foo exposing ( Bar )" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (PortModule { moduleName = Node.empty <| [ "Foo" ], exposingList = Node empty <| Explicit [ Node empty <| TypeOrAliasExpose "Bar" ] }))
        , test "effect moduleDefinition" <|
            \() ->
                parseFullStringWithNullState "effect module Foo where {command = MyCmd, subscription = MySub } exposing (Bar)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal
                        (Just
                            (EffectModule
                                { moduleName = Node empty <| [ "Foo" ]
                                , exposingList = Node empty <| Explicit [ Node empty <| TypeOrAliasExpose "Bar" ]
                                , command = Just <| Node empty <| "MyCmd"
                                , subscription = Just <| Node empty <| "MySub"
                                }
                            )
                        )
        , test "unformatted" <|
            \() ->
                parseFullStringWithNullState "module \n Foo \n exposing  (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node empty <| [ "Foo" ], exposingList = Node empty <| All empty }))
        , test "unformatted wrong" <|
            \() ->
                parseFullStringWithNullState "module \nFoo \n exposing  (..)" Parser.moduleDefinition
                    |> Expect.equal Nothing
        , test "exposing all" <|
            \() ->
                parseFullStringWithNullState "module Foo exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node empty <| [ "Foo" ], exposingList = Node empty <| All empty }))
        , test "module name with _" <|
            \() ->
                parseFullStringWithNullState "module I_en_gb exposing (..)" Parser.moduleDefinition
                    |> Maybe.map noRangeModule
                    |> Expect.equal (Just (NormalModule { moduleName = Node empty <| [ "I_en_gb" ], exposingList = Node empty <| All empty }))
        , test "Regression test for Incorrect range in if expression" <|
            \() ->
                parseFullStringWithNullState
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
                                                { arguments = [], expression = Node { start = { row = 13, column = 5 }, end = { row = 13, column = 6 } } (Integer 3), name = Node { start = { row = 13, column = 1 }, end = { row = 13, column = 2 } } "b" }
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
                parseFullStringWithNullState
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
        ]
