module Elm.Parser.ExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expression exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ExpressionTests"
        [ test "empty" <|
            \() ->
                parseFullStringWithNullState "" expression
                    |> Expect.equal Nothing
        , test "Integer literal" <|
            \() ->
                parseFullStringWithNullState "101" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (IntegerLiteral 101))
        , test "String literal" <|
            \() ->
                parseFullStringWithNullState "\"Bar\"" expression
                    |> expectAst (Node { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } } (StringLiteral SingleQuote "Bar"))
        , test "character literal" <|
            \() ->
                parseFullStringWithNullState "'c'" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (CharLiteral 'c'))
        , test "tuple expression" <|
            \() ->
                parseFullStringWithNullState "(1,2)" expression
                    |> expectAst
                        (Node { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } }
                            (TupleExpression
                                [ Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (IntegerLiteral 1)
                                , Node { end = { column = 5, row = 1 }, start = { column = 4, row = 1 } } (IntegerLiteral 2)
                                ]
                            )
                        )
        , test "prefix expression" <|
            \() ->
                parseFullStringWithNullState "(,)" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (PrefixOperator ","))
        , test "String literal multiline" <|
            \() ->
                parseFullStringWithNullState "\"\"\"Bar foo \n a\"\"\"" expression
                    |> expectAst (Node { end = { column = 6, row = 2 }, start = { column = 1, row = 1 } } (StringLiteral TripleQuote "Bar foo \n a"))
        , test "Regression test for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\{\\}\"\"\"" expression
                    |> Expect.equal Nothing
        , test "Regression test 2 for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\{\\\\}\"\"\"" expression
                    |> expectAst (Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } } (StringLiteral TripleQuote "\\{\\}"))
        , test "Regression test 3 for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\a-blablabla-\\\\b\"\"\"" expression
                    |> expectAst (Node { end = { column = 24, row = 1 }, start = { column = 1, row = 1 } } (StringLiteral TripleQuote "\\a-blablabla-\\b"))
        , test "Type expression for upper case" <|
            \() ->
                parseFullStringWithNullState "Bar" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                parseFullStringWithNullState "bar" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                parseFullStringWithNullState "Bar.foo" expression
                    |> expectAst (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [ "Bar" ] "foo"))
        , test "parenthesizedExpression" <|
            \() ->
                parseFullStringWithNullState "(bar)" expression
                    |> expectAst
                        (Node { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } }
                            (TupleExpression
                                [ Node { end = { column = 5, row = 1 }, start = { column = 2, row = 1 } } (FunctionOrValue [] "bar")
                                ]
                            )
                        )
        , test "application expression" <|
            \() ->
                parseFullStringWithNullState "List.concat []" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                            FunctionCall
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListLiteral []
                                ]
                        )
        , test "application expression with operator" <|
            \() ->
                parseFullStringWithNullState "model + 1" expression
                    |> expectAst
                        (Node (Range { column = 1, row = 1 } { column = 10, row = 1 }) <|
                            FunctionCall
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } <| FunctionOrValue [] "model")
                                [ Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } <| Operator "+"
                                , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| IntegerLiteral 1
                                ]
                        )
        , test "application expression 2" <|
            \() ->
                parseFullStringWithNullState "(\"\", always (List.concat [ [ fileName ], [] ]))" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } } <|
                            TupleExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } <| StringLiteral SingleQuote ""
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } } <|
                                    FunctionCall
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } <| FunctionOrValue [] "always")
                                        [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } } <|
                                            TupleExpression
                                                [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } } <|
                                                    FunctionCall
                                                        (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } <|
                                                            FunctionOrValue [ "List" ] "concat"
                                                        )
                                                        [ Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } } <|
                                                            ListLiteral
                                                                [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } } <|
                                                                    ListLiteral
                                                                        [ Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } <|
                                                                            FunctionOrValue [] "fileName"
                                                                        ]
                                                                , Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } <|
                                                                    ListLiteral []
                                                                ]
                                                        ]
                                                ]
                                        ]
                                ]
                        )
        , test "expressionNotApplication simple" <|
            \() ->
                parseFullStringWithNullState "foo" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "foo"))
        , test "unit application" <|
            \() ->
                parseFullStringWithNullState "Task.succeed ()" expression
                    |> expectAst
                        (Node { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall
                                (Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [ "Task" ] "succeed"))
                                [ Node { end = { column = 16, row = 1 }, start = { column = 14, row = 1 } } (TupleExpression []) ]
                            )
                        )
        , test "Function call" <|
            \() ->
                parseFullStringWithNullState "foo bar" expression
                    |> expectAst
                        (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall
                                (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "foo"))
                                [ Node { end = { column = 8, row = 1 }, start = { column = 5, row = 1 } } (FunctionOrValue [] "bar") ]
                            )
                        )
        , test "ifBlockExpression" <|
            \() ->
                parseFullStringWithNullState "if True then foo else bar" expression
                    |> expectAst
                        (Node { end = { column = 26, row = 1 }, start = { column = 1, row = 1 } }
                            (If
                                (Node { end = { column = 8, row = 1 }, start = { column = 4, row = 1 } } (FunctionOrValue [] "True"))
                                (Node { end = { column = 17, row = 1 }, start = { column = 14, row = 1 } } (FunctionOrValue [] "foo"))
                                (Node { end = { column = 26, row = 1 }, start = { column = 23, row = 1 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "nestedIfExpression" <|
            \() ->
                parseFullStringWithNullState "if True then if False then foo else baz else bar" expression
                    |> expectAst
                        (Node { end = { column = 49, row = 1 }, start = { column = 1, row = 1 } }
                            (If
                                (Node { end = { column = 8, row = 1 }, start = { column = 4, row = 1 } } (FunctionOrValue [] "True"))
                                (Node { end = { column = 40, row = 1 }, start = { column = 14, row = 1 } }
                                    (If
                                        (Node { end = { column = 22, row = 1 }, start = { column = 17, row = 1 } } (FunctionOrValue [] "False"))
                                        (Node { end = { column = 31, row = 1 }, start = { column = 28, row = 1 } } (FunctionOrValue [] "foo"))
                                        (Node { end = { column = 40, row = 1 }, start = { column = 37, row = 1 } } (FunctionOrValue [] "baz"))
                                    )
                                )
                                (Node { end = { column = 49, row = 1 }, start = { column = 46, row = 1 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "recordExpression" <|
            \() ->
                parseFullStringWithNullState "{ model = 0, view = view, update = update }" expression
                    |> expectAst
                        (Node { end = { column = 44, row = 1 }, start = { column = 1, row = 1 } }
                            (Record
                                [ Node { end = { column = 12, row = 1 }, start = { column = 3, row = 1 } }
                                    ( Node { end = { column = 8, row = 1 }, start = { column = 3, row = 1 } } "model"
                                    , Node { end = { column = 12, row = 1 }, start = { column = 11, row = 1 } } (IntegerLiteral 0)
                                    )
                                , Node { end = { column = 25, row = 1 }, start = { column = 14, row = 1 } }
                                    ( Node { end = { column = 18, row = 1 }, start = { column = 14, row = 1 } } "view"
                                    , Node { end = { column = 25, row = 1 }, start = { column = 21, row = 1 } } (FunctionOrValue [] "view")
                                    )
                                , Node { end = { column = 43, row = 1 }, start = { column = 27, row = 1 } }
                                    ( Node { end = { column = 33, row = 1 }, start = { column = 27, row = 1 } } "update"
                                    , Node { end = { column = 42, row = 1 }, start = { column = 36, row = 1 } } (FunctionOrValue [] "update")
                                    )
                                ]
                            )
                        )
        , test "recordExpression with comment" <|
            \() ->
                parseFullStringWithNullState "{ foo = 1 -- bar\n , baz = 2 }" expression
                    |> expectAst
                        (Node { end = { column = 13, row = 2 }, start = { column = 1, row = 1 } }
                            (Record
                                [ Node { end = { column = 10, row = 1 }, start = { column = 3, row = 1 } }
                                    ( Node { end = { column = 6, row = 1 }, start = { column = 3, row = 1 } } "foo"
                                    , Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } } (IntegerLiteral 1)
                                    )
                                , Node { end = { column = 12, row = 2 }, start = { column = 4, row = 2 } }
                                    ( Node { end = { column = 7, row = 2 }, start = { column = 4, row = 2 } } "baz"
                                    , Node { end = { column = 11, row = 2 }, start = { column = 10, row = 2 } } (IntegerLiteral 2)
                                    )
                                ]
                            )
                        )
        , test "listExpression" <|
            \() ->
                parseFullStringWithNullState "[ class \"a\", text \"Foo\"]" expression
                    |> expectAst
                        (Node { end = { column = 25, row = 1 }, start = { column = 1, row = 1 } }
                            (ListLiteral
                                [ Node { end = { column = 12, row = 1 }, start = { column = 3, row = 1 } }
                                    (FunctionCall
                                        (Node { end = { column = 8, row = 1 }, start = { column = 3, row = 1 } } (FunctionOrValue [] "class"))
                                        [ Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } } (StringLiteral SingleQuote "a") ]
                                    )
                                , Node { end = { column = 24, row = 1 }, start = { column = 14, row = 1 } }
                                    (FunctionCall
                                        (Node { end = { column = 18, row = 1 }, start = { column = 14, row = 1 } } (FunctionOrValue [] "text"))
                                        [ Node { end = { column = 24, row = 1 }, start = { column = 19, row = 1 } } (StringLiteral SingleQuote "Foo") ]
                                    )
                                ]
                            )
                        )
        , test "listExpression singleton with comment" <|
            \() ->
                parseFullStringWithNullState "[ 1 {- Foo-} ]" expression
                    |> expectAst
                        (Node { end = { column = 15, row = 1 }, start = { column = 1, row = 1 } }
                            (ListLiteral
                                [ Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } (IntegerLiteral 1)
                                ]
                            )
                        )
        , test "listExpression empty with comment" <|
            \() ->
                parseFullStringWithNullState "[{-| Foo -}]" expression
                    |> expectAst (Node { end = { column = 13, row = 1 }, start = { column = 1, row = 1 } } (ListLiteral []))
        , test "qualified expression" <|
            \() ->
                parseFullStringWithNullState "Html.text" expression
                    |> expectAst (Node { end = { column = 10, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [ "Html" ] "text"))
        , test "record access" <|
            \() ->
                parseFullStringWithNullState "foo.bar" expression
                    |> expectAst
                        (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } }
                            (RecordAccess
                                (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "foo"))
                                (Node { end = { column = 8, row = 1 }, start = { column = 5, row = 1 } } "bar")
                            )
                        )
        , test "multiple record access operations" <|
            \() ->
                parseFullStringWithNullState "foo.bar.baz" expression
                    |> expectAst
                        (Node { end = { column = 12, row = 1 }, start = { column = 1, row = 1 } }
                            (RecordAccess
                                (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } }
                                    (RecordAccess
                                        (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "foo"))
                                        (Node { end = { column = 8, row = 1 }, start = { column = 5, row = 1 } } "bar")
                                    )
                                )
                                (Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } } "baz")
                            )
                        )
        , test "multiple record access operations with module name" <|
            \() ->
                parseFullStringWithNullState "A.B.foo.bar.baz" expression
                    |> expectAst
                        (Node { end = { column = 16, row = 1 }, start = { column = 1, row = 1 } }
                            (RecordAccess
                                (Node { end = { column = 12, row = 1 }, start = { column = 1, row = 1 } }
                                    (RecordAccess
                                        (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [ "A", "B" ] "foo"))
                                        (Node { end = { column = 12, row = 1 }, start = { column = 9, row = 1 } } "bar")
                                    )
                                )
                                (Node { end = { column = 16, row = 1 }, start = { column = 13, row = 1 } } "baz")
                            )
                        )
        , test "record update" <|
            \() ->
                parseFullStringWithNullState "{ model | count = 1, loading = True }" expression
                    |> expectAst
                        (Node { end = { column = 38, row = 1 }, start = { column = 1, row = 1 } }
                            (RecordUpdate
                                (Node { end = { column = 8, row = 1 }, start = { column = 3, row = 1 } } "model")
                                (Node { end = { column = 20, row = 1 }, start = { column = 11, row = 1 } }
                                    ( Node { end = { column = 16, row = 1 }, start = { column = 11, row = 1 } } "count"
                                    , Node { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } } (IntegerLiteral 1)
                                    )
                                )
                                [ Node { end = { column = 37, row = 1 }, start = { column = 22, row = 1 } }
                                    ( Node { end = { column = 29, row = 1 }, start = { column = 22, row = 1 } } "loading"
                                    , Node { end = { column = 36, row = 1 }, start = { column = 32, row = 1 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                parseFullStringWithNullState "{model| count = 1, loading = True }" expression
                    |> expectAst
                        (Node { end = { column = 36, row = 1 }, start = { column = 1, row = 1 } }
                            (RecordUpdate
                                (Node { end = { column = 7, row = 1 }, start = { column = 2, row = 1 } } "model")
                                (Node { end = { column = 18, row = 1 }, start = { column = 9, row = 1 } }
                                    ( Node { end = { column = 14, row = 1 }, start = { column = 9, row = 1 } } "count"
                                    , Node { end = { column = 18, row = 1 }, start = { column = 17, row = 1 } } (IntegerLiteral 1)
                                    )
                                )
                                [ Node { end = { column = 35, row = 1 }, start = { column = 20, row = 1 } }
                                    ( Node { end = { column = 27, row = 1 }, start = { column = 20, row = 1 } } "loading"
                                    , Node { end = { column = 34, row = 1 }, start = { column = 30, row = 1 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record access as function" <|
            \() ->
                parseFullStringWithNullState "List.map .name people" expression
                    |> expectAst
                        (Node { end = { column = 22, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall (Node { end = { column = 9, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [ "List" ] "map"))
                                [ Node { end = { column = 15, row = 1 }, start = { column = 10, row = 1 } } (RecordAccessFunction "name")
                                , Node { end = { column = 22, row = 1 }, start = { column = 16, row = 1 } } (FunctionOrValue [] "people")
                                ]
                            )
                        )
        , test "record access direct" <|
            \() ->
                parseFullStringWithNullState "(.spaceEvenly Internal.Style.classes)" expression
                    |> expectAst
                        (Node { end = { column = 38, row = 1 }, start = { column = 1, row = 1 } }
                            (TupleExpression
                                [ Node { end = { column = 37, row = 1 }, start = { column = 2, row = 1 } }
                                    (FunctionCall (Node { end = { column = 14, row = 1 }, start = { column = 2, row = 1 } } (RecordAccessFunction "spaceEvenly"))
                                        [ Node { end = { column = 37, row = 1 }, start = { column = 15, row = 1 } } (FunctionOrValue [ "Internal", "Style" ] "classes")
                                        ]
                                    )
                                ]
                            )
                        )
        , test "positive integer should be invalid" <|
            \() ->
                parseFullStringWithNullState "+1" expression
                    |> Expect.equal Nothing
        , test "expression ending with an operator should not be valid" <|
            \() ->
                parseFullStringWithNullState "1++" expression
                    |> Expect.equal Nothing
        , test "prefix notation" <|
            \() ->
                parseFullStringWithNullState "(::) x" expression
                    |> expectAst
                        (Node { end = { column = 7, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall
                                (Node { end = { column = 5, row = 1 }, start = { column = 1, row = 1 } } (PrefixOperator "::"))
                                [ Node { end = { column = 7, row = 1 }, start = { column = 6, row = 1 } } (FunctionOrValue [] "x") ]
                            )
                        )
        , test "negated expression for value" <|
            \() ->
                parseFullStringWithNullState "-x" expression
                    |> expectAst
                        (Node { end = { column = 3, row = 1 }, start = { column = 1, row = 1 } }
                            (Negation (Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (FunctionOrValue [] "x")))
                        )
        , test "negated expression in application" <|
            \() ->
                parseFullStringWithNullState "toFloat -5" expression
                    |> expectAst
                        (Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall
                                (Node { end = { column = 8, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "toFloat"))
                                [ Node { end = { column = 11, row = 1 }, start = { column = 9, row = 1 } }
                                    (Negation (Node { end = { column = 11, row = 1 }, start = { column = 10, row = 1 } } (IntegerLiteral 5)))
                                ]
                            )
                        )
        , test "negated expression for parenthesized" <|
            \() ->
                parseFullStringWithNullState "-(x - y)" expression
                    |> expectAst
                        (Node { end = { column = 9, row = 1 }, start = { column = 1, row = 1 } }
                            (Negation
                                (Node { end = { column = 9, row = 1 }, start = { column = 2, row = 1 } }
                                    (TupleExpression
                                        [ Node { end = { column = 8, row = 1 }, start = { column = 3, row = 1 } }
                                            (FunctionCall
                                                (Node { end = { column = 4, row = 1 }, start = { column = 3, row = 1 } } (FunctionOrValue [] "x"))
                                                [ Node { end = { column = 7, row = 1 }, start = { column = 5, row = 1 } } (Operator "-")
                                                , Node { end = { column = 8, row = 1 }, start = { column = 7, row = 1 } } (FunctionOrValue [] "y")
                                                ]
                                            )
                                        ]
                                    )
                                )
                            )
                        )
        , test "function with higher order" <|
            \() ->
                parseFullStringWithNullState "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')" expression
                    |> expectAst
                        (Node { end = { column = 54, row = 1 }, start = { column = 1, row = 1 } }
                            (FunctionCall
                                (Node { end = { column = 11, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "chompWhile"))
                                [ Node { end = { column = 54, row = 1 }, start = { column = 12, row = 1 } }
                                    (TupleExpression
                                        [ Node { end = { column = 53, row = 1 }, start = { column = 13, row = 1 } }
                                            (LambdaExpression
                                                { expression =
                                                    Node { end = { column = 53, row = 1 }, start = { column = 19, row = 1 } }
                                                        (FunctionCall
                                                            (Node { end = { column = 20, row = 1 }, start = { column = 19, row = 1 } } (FunctionOrValue [] "c"))
                                                            [ Node { end = { column = 23, row = 1 }, start = { column = 21, row = 1 } } (Operator "==")
                                                            , Node { end = { column = 27, row = 1 }, start = { column = 24, row = 1 } } (CharLiteral ' ')
                                                            , Node { end = { column = 30, row = 1 }, start = { column = 28, row = 1 } } (Operator "||")
                                                            , Node { end = { column = 32, row = 1 }, start = { column = 31, row = 1 } } (FunctionOrValue [] "c")
                                                            , Node { end = { column = 35, row = 1 }, start = { column = 33, row = 1 } } (Operator "==")
                                                            , Node { end = { column = 40, row = 1 }, start = { column = 36, row = 1 } } (CharLiteral '\n')
                                                            , Node { end = { column = 43, row = 1 }, start = { column = 41, row = 1 } } (Operator "||")
                                                            , Node { end = { column = 45, row = 1 }, start = { column = 44, row = 1 } } (FunctionOrValue [] "c")
                                                            , Node { end = { column = 48, row = 1 }, start = { column = 46, row = 1 } } (Operator "==")
                                                            , Node { end = { column = 53, row = 1 }, start = { column = 49, row = 1 } } (CharLiteral '\u{000D}')
                                                            ]
                                                        )
                                                , firstArg = Node { end = { column = 15, row = 1 }, start = { column = 14, row = 1 } } (VarPattern_ "c")
                                                , restOfArgs = []
                                                }
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
        ]


expectAst : Node Expression -> Maybe (Node Expression) -> Expect.Expectation
expectAst expected result =
    case result of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected
