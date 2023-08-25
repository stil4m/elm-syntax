module Elm.Parser.PatternTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Patterns as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "PatternTests"
        [ test "unit pattern" <|
            \() ->
                "()"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } UnitPattern)
        , test "string pattern" <|
            \() ->
                "\"Foo\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (StringPattern "Foo"))
        , test "multiple patterns" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.pattern
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just (VarPattern "a"))
        , test "char pattern" <|
            \() ->
                "'f'"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (CharPattern 'f'))
        , test "qualified pattern" <|
            \() ->
                parseFullStringState emptyState "X x" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                                (NamedPattern (QualifiedNameRef [] "X")
                                    [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (VarPattern "x") ]
                                )
                            )
                        )
        , test "qualified pattern without and with spacing should parse to the same" <|
            \() ->
                let
                    a =
                        parseFullStringWithNullState "Bar " Parser.pattern

                    b =
                        parseFullStringWithNullState "Bar" Parser.pattern
                in
                a
                    |> Expect.equal b
        , test "all pattern" <|
            \() ->
                "_"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } AllPattern)
        , test "non cons pattern " <|
            \() ->
                parseFullStringState emptyState "(X x)" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                                (ParenthesizedPattern
                                    (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } }
                                        (NamedPattern (QualifiedNameRef [] "X")
                                            [ Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (VarPattern "x") ]
                                        )
                                    )
                                )
                            )
                        )
        , test "uncons with parens pattern" <|
            \() ->
                parseFullStringState emptyState "(X x) :: xs" Parser.pattern
                    |> Expect.equal
                        (Just
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
                        )
        , test "int pattern" <|
            \() ->
                "1"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } <| IntPattern 1)
        , test "uncons pattern" <|
            \() ->
                parseFullStringState emptyState "n :: tail" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } <|
                                UnConsPattern (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } <| VarPattern "n")
                                    (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 10 } } <| VarPattern "tail")
                            )
                        )
        , test "list pattern" <|
            \() ->
                parseFullStringState emptyState "[1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } <|
                                ListPattern [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } <| IntPattern 1 ]
                            )
                        )
        , test "empty list pattern" <|
            \() ->
                "[]"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (ListPattern []))
        , test "empty list pattern with whitespace" <|
            \() ->
                "[ ]"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (ListPattern []))
        , test "single element list pattern with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState "[1 ]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <|
                                ListPattern [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } <| IntPattern 1 ]
                            )
                        )
        , test "single element list pattern with leading whitespace" <|
            \() ->
                parseFullStringState emptyState "[ 1]" Parser.pattern
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <|
                                ListPattern [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } <| IntPattern 1 ]
                            )
                        )
        , test "float pattern" <|
            \() ->
                "1.2"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FloatPattern 1.2))
        , test "record pattern" <|
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
        , test "record pattern with whitespace" <|
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
        , test "record pattern with trailing whitespace" <|
            \() ->
                "{a }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } "a" ]
                            )
                        )
        , test "record pattern with leading whitespace" <|
            \() ->
                "{ a}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (RecordPattern
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } "a" ]
                            )
                        )
        , test "empty record pattern" <|
            \() ->
                "{}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } (RecordPattern []))
        , test "empty record pattern with whitespace" <|
            \() ->
                "{ }"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (RecordPattern []))
        , test "named pattern" <|
            \() ->
                "True"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } }
                            (NamedPattern { moduleName = [], name = "True" } [])
                        )
        , test "tuple pattern" <|
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
        , test "destructure pattern" <|
            \() ->
                "Set x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (NamedPattern
                                { moduleName = [], name = "Set" }
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                            )
                        )
        , test "tuple pattern 2" <|
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
        , test "record as pattern" <|
            \() ->
                parseFullStringState emptyState "{model,context} as appState" Parser.pattern
                    |> Expect.equal
                        (Just
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
                        )
        , test "complex pattern" <|
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
        , test "complex pattern 2" <|
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
    case parseFullStringWithNullState source Parser.pattern of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected
