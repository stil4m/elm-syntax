module Elm.Parser.PatternTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Patterns as Parser
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "PatternTests"
        [ test "Unit" <|
            \() ->
                "()"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } UnitPattern)
        , test "String" <|
            \() ->
                "\"Foo\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (StringPattern "Foo"))
        , test "Char" <|
            \() ->
                "'f'"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (CharPattern 'f'))
        , test "Wildcard" <|
            \() ->
                "_"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } AllPattern)
        , test "Parenthesized" <|
            \() ->
                "(x)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (ParenthesizedPattern
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (VarPattern "x"))
                            )
                        )
        , test "Int" <|
            \() ->
                "1"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } <| IntPattern 1)
        , test "Hex int" <|
            \() ->
                "0x1"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } <| HexPattern 1)
        , test "Float should not be valid" <|
            \() -> expectInvalid "1.0"
        , test "Uncons" <|
            \() ->
                "n :: tail"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } <|
                            UnConsPattern (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } <| VarPattern "n")
                                (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } <| VarPattern "tail")
                        )
        , test "Uncons with parens" <|
            \() ->
                "(X x) :: xs"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <|
                            UnConsPattern
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } <|
                                    ParenthesizedPattern
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } <|
                                            NamedPattern (QualifiedNameRef [] "X")
                                                [ Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } <| VarPattern "x" ]
                                        )
                                )
                                (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } <| VarPattern "xs")
                        )
        , test "Empty list" <|
            \() ->
                "[]"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (ListPattern []))
        , test "Empty list pattern with whitespace" <|
            \() ->
                "[ ]"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (ListPattern []))
        , test "Single element list" <|
            \() ->
                "[1]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } <|
                            ListPattern [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } <| IntPattern 1 ]
                        )
        , test "Single element list with trailing whitespace" <|
            \() ->
                "[1 ]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <|
                            ListPattern [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } <| IntPattern 1 ]
                        )
        , test "Single element list with leading whitespace" <|
            \() ->
                "[ 1]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <|
                            ListPattern [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } <| IntPattern 1 ]
                        )
        , test "Empty record" <|
            \() ->
                "{}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (RecordPattern []))
        , test "Empty record with whitespace" <|
            \() ->
                "{ }"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (RecordPattern []))
        , test "Record" <|
            \() ->
                "{a,b}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } "b"
                                ]
                            )
                        )
        , test "Record pattern with whitespace" <|
            \() ->
                "{a , b}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a"
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "b"
                                ]
                            )
                        )
        , test "Record pattern with trailing whitespace" <|
            \() ->
                "{a }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                            )
                        )
        , test "Record pattern with leading whitespace" <|
            \() ->
                "{ a}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                            )
                        )
        , test "Named" <|
            \() ->
                "True"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (NamedPattern { moduleName = [], name = "True" } [])
                        )
        , test "Named pattern without and with spacing should parse to the same" <|
            \() ->
                parseFullString "Bar " Parser.pattern
                    |> Expect.equal (parseFullString "Bar" Parser.pattern)
        , test "Qualified named" <|
            \() ->
                "Basics.True"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (NamedPattern { moduleName = [ "Basics" ], name = "True" } [])
                        )
        , test "Named pattern with data" <|
            \() ->
                "Set x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (NamedPattern
                                { moduleName = [], name = "Set" }
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                            )
                        )
        , test "Qualified named pattern with data" <|
            \() ->
                "Set.Set x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (NamedPattern
                                { moduleName = [ "Set" ], name = "Set" }
                                [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (VarPattern "x") ]
                            )
                        )
        , test "Tuple" <|
            \() ->
                "(model, cmd)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (TuplePattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (VarPattern "model")
                                , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (VarPattern "cmd")
                                ]
                            )
                        )
        , test "Nested tuple" <|
            \() ->
                "(a,{b,c},())"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } }
                            (TuplePattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (VarPattern "a")
                                , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 9 } } (RecordPattern [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } "b", Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "c" ])
                                , Node { start = { row = 1, column = 10 }, end = { row = 1, column = 12 } } UnitPattern
                                ]
                            )
                        )
        , test "As pattern" <|
            \() ->
                "x as y"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (AsPattern
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (VarPattern "x"))
                                (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "y")
                            )
                        )
        , test "should fail to parse when right side is not a direct variable name" <|
            \() ->
                "x as (y)"
                    |> expectInvalid
        , test "should fail to parse when right side is an invalid variable name" <|
            \() ->
                "x as _y"
                    |> expectInvalid
        , test "should fail to parse when right side is not a variable name" <|
            \() ->
                "x as 1"
                    |> expectInvalid
        , test "Record as" <|
            \() ->
                "{model,context} as appState"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 28 } } <|
                            AsPattern
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } } <|
                                    RecordPattern
                                        [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model"
                                        , Node { start = { row = 1, column = 8 }, end = { row = 1, column = 15 } } "context"
                                        ]
                                )
                                (Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } "appState")
                        )
        , test "Complex" <|
            \() ->
                "(Index irec as index, docVector)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 33 } }
                            (TuplePattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 21 } }
                                    (AsPattern
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 12 } }
                                            (NamedPattern { moduleName = [], name = "Index" }
                                                [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 12 } } (VarPattern "irec") ]
                                            )
                                        )
                                        (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 21 } } "index")
                                    )
                                , Node { start = { row = 1, column = 23 }, end = { row = 1, column = 32 } } (VarPattern "docVector")
                                ]
                            )
                        )
        , test "Complex pattern 2" <|
            \() ->
                "RBNode_elm_builtin col (RBNode_elm_builtin Red  (RBNode_elm_builtin Red xv))"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 77 } }
                            (NamedPattern { moduleName = [], name = "RBNode_elm_builtin" }
                                [ Node { start = { row = 1, column = 20 }, end = { row = 1, column = 23 } } (VarPattern "col")
                                , Node { start = { row = 1, column = 24 }, end = { row = 1, column = 77 } }
                                    (ParenthesizedPattern
                                        (Node { start = { row = 1, column = 25 }, end = { row = 1, column = 76 } }
                                            (NamedPattern { moduleName = [], name = "RBNode_elm_builtin" }
                                                [ Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } (NamedPattern { moduleName = [], name = "Red" } [])
                                                , Node { start = { row = 1, column = 49 }, end = { row = 1, column = 76 } } (ParenthesizedPattern (Node { start = { row = 1, column = 50 }, end = { row = 1, column = 75 } } (NamedPattern { moduleName = [], name = "RBNode_elm_builtin" } [ Node { start = { row = 1, column = 69 }, end = { row = 1, column = 72 } } (NamedPattern { moduleName = [], name = "Red" } []), Node { start = { row = 1, column = 73 }, end = { row = 1, column = 75 } } (VarPattern "xv") ])))
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
        ]


expectAst : Node Pattern -> String -> Expect.Expectation
expectAst expected source =
    case parseFullString source Parser.pattern of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parseFullString source Parser.pattern of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
