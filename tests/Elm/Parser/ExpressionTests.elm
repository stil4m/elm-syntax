module Elm.Parser.ExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (parseFullStringWithNullState)
import Elm.Parser.Declarations exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (Test, describe, test)


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
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Integer 101))
        , test "String literal" <|
            \() ->
                parseFullStringWithNullState "\"Bar\"" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Literal "Bar"))
        , test "character literal" <|
            \() ->
                parseFullStringWithNullState "'c'" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (CharLiteral 'c'))
        , test "tuple expression" <|
            \() ->
                parseFullStringWithNullState "(1,2)" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (TupledExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Integer 1)
                                , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Integer 2)
                                ]
                            )
                        )
        , test "prefix expression" <|
            \() ->
                parseFullStringWithNullState "(,)" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (PrefixOperator ","))
        , test "String literal multiline" <|
            \() ->
                parseFullStringWithNullState "\"\"\"Bar foo \n a\"\"\"" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Literal "Bar foo \n a"))
        , test "Regression test for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\{\\}\"\"\"" expression
                    |> Expect.equal Nothing
        , test "Regression test 2 for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\{\\\\}\"\"\"" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Literal "\\{\\}"))
        , test "Regression test 3 for multiline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\a-blablabla-\\\\b\"\"\"" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Literal "\\a-blablabla-\\b"))
        , test "Type expression for upper case" <|
            \() ->
                parseFullStringWithNullState "Bar" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                parseFullStringWithNullState "bar" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                parseFullStringWithNullState "Bar.foo" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [ "Bar" ] "foo"))
        , test "parenthesizedExpression" <|
            \() ->
                parseFullStringWithNullState "(bar)" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (ParenthesizedExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "application expression" <|
            \() ->
                parseFullStringWithNullState "List.concat []" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                            Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat"
                                , Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListExpr []
                                ]
                        )
        , test "application expression with operator" <|
            \() ->
                parseFullStringWithNullState "model + 1" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } <|
                            Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } <| FunctionOrValue [] "model"
                                , Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } <| Operator "+"
                                , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| Integer 1
                                ]
                        )
        , test "application expression 2" <|
            \() ->
                parseFullStringWithNullState "(\"\", always (List.concat [ [ fileName ], [] ]))" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } } <|
                            TupledExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } <| Literal ""
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } } <|
                                    Application
                                        [ Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } <| FunctionOrValue [] "always"
                                        , Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } } <|
                                            ParenthesizedExpression
                                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } } <|
                                                    Application
                                                        [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } <|
                                                            FunctionOrValue [ "List" ] "concat"
                                                        , Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } } <|
                                                            ListExpr
                                                                [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } } <|
                                                                    ListExpr
                                                                        [ Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } <|
                                                                            FunctionOrValue [] "fileName"
                                                                        ]
                                                                , Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } <|
                                                                    ListExpr []
                                                                ]
                                                        ]
                                                )
                                        ]
                                ]
                        )
        , test "expressionNotApplication simple" <|
            \() ->
                parseFullStringWithNullState "foo" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
        , test "unit application" <|
            \() ->
                parseFullStringWithNullState "Task.succeed ()" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (FunctionOrValue [ "Task" ] "succeed")
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } UnitExpr
                                ]
                            )
                        )
        , test "Function call" <|
            \() ->
                parseFullStringWithNullState "foo bar" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo")
                                , Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "bar")
                                ]
                            )
                        )
        , test "ifBlockExpression" <|
            \() ->
                parseFullStringWithNullState "if True then foo else bar" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            (IfBlock
                                (Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "True"))
                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (FunctionOrValue [] "foo"))
                                (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "nestedIfExpression" <|
            \() ->
                parseFullStringWithNullState "if True then if False then foo else baz else bar" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                            (IfBlock
                                (Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "True"))
                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                    (IfBlock
                                        (Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } (FunctionOrValue [] "False"))
                                        (Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (FunctionOrValue [] "foo"))
                                        (Node { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } } (FunctionOrValue [] "baz"))
                                    )
                                )
                                (Node { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "recordExpression" <|
            \() ->
                parseFullStringWithNullState "{ model = 0, view = view, update = update }" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                            (RecordExpr
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model"
                                    , Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (Integer 0)
                                    )
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                    ( Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "view"
                                    , Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } (FunctionOrValue [] "view")
                                    )
                                , Node { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                    ( Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } } "update"
                                    , Node { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } } (FunctionOrValue [] "update")
                                    )
                                ]
                            )
                        )
        , test "recordExpression with comment" <|
            \() ->
                parseFullStringWithNullState "{ foo = 1 -- bar\n , baz = 2 }" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                            (RecordExpr
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                    ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Integer 1)
                                    )
                                , Node { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                    ( Node { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } } "baz"
                                    , Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } (Integer 2)
                                    )
                                ]
                            )
                        )
        , test "listExpression" <|
            \() ->
                parseFullStringWithNullState "[ class \"a\", text \"Foo\"]" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            (ListExpr
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    (Application
                                        [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "class")
                                        , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Literal "a")
                                        ]
                                    )
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                    (Application
                                        [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (FunctionOrValue [] "text")
                                        , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (Literal "Foo")
                                        ]
                                    )
                                ]
                            )
                        )
        , test "listExpression singleton with comment" <|
            \() ->
                parseFullStringWithNullState "[ 1 {- Foo-} ]" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (ListExpr
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Integer 1)
                                ]
                            )
                        )
        , test "listExpression empty with comment" <|
            \() ->
                parseFullStringWithNullState "[{-| Foo -}]" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (ListExpr []))
        , test "qualified expression" <|
            \() ->
                parseFullStringWithNullState "Html.text" expression
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (FunctionOrValue [ "Html" ] "text"))
        , test "record access" <|
            \() ->
                parseFullStringWithNullState "foo.bar" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (RecordAccess
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                            )
                        )
        , test "multiple record access operations" <|
            \() ->
                parseFullStringWithNullState "foo.bar.baz" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (RecordAccess
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                    (RecordAccess
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                        (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                    )
                                )
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "baz")
                            )
                        )
        , test "multiple record access operations with module name" <|
            \() ->
                parseFullStringWithNullState "A.B.foo.bar.baz" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (RecordAccess
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                    (RecordAccess
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [ "A", "B" ] "foo"))
                                        (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "bar")
                                    )
                                )
                                (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } "baz")
                            )
                        )
        , test "record update" <|
            \() ->
                parseFullStringWithNullState "{ model | count = 1, loading = True }" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (RecordUpdateExpression
                                (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                    ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                    , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Integer 1)
                                    )
                                , Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                    ( Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                    , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                parseFullStringWithNullState "{model| count = 1, loading = True }" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            (RecordUpdateExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                    ( Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                    , Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Integer 1)
                                    )
                                , Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                    ( Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                    , Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record access as function" <|
            \() ->
                parseFullStringWithNullState "List.map .name people" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (FunctionOrValue [ "List" ] "map")
                                , Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (RecordAccessFunction ".name")
                                , Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (FunctionOrValue [] "people")
                                ]
                            )
                        )
        , test "record access direct" <|
            \() ->
                parseFullStringWithNullState "(.spaceEvenly Internal.Style.classes)" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (ParenthesizedExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                    (Application
                                        [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (RecordAccessFunction ".spaceEvenly")
                                        , Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (FunctionOrValue [ "Internal", "Style" ] "classes")
                                        ]
                                    )
                                )
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
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (PrefixOperator "::")
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (FunctionOrValue [] "x")
                                ]
                            )
                        )
        , test "negated expression for value" <|
            \() ->
                parseFullStringWithNullState "-x" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            (Negation (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (FunctionOrValue [] "x")))
                        )
        , test "negated expression in application" <|
            \() ->
                parseFullStringWithNullState "toFloat -5" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "toFloat")
                                , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                    (Negation (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (Integer 5)))
                                ]
                            )
                        )
        , test "negated expression for parenthesized" <|
            \() ->
                parseFullStringWithNullState "-(x - y)" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (Negation
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 9 } }
                                    (ParenthesizedExpression
                                        (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                            (Application
                                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "x")
                                                , Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } (Operator "-")
                                                , Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "y")
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
                        )
        , test "function with higher order" <|
            \() ->
                parseFullStringWithNullState "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')" expression
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                            (Application
                                [ Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "chompWhile")
                                , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                    (ParenthesizedExpression
                                        (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                            (LambdaExpression
                                                { expression =
                                                    Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                        (Application
                                                            [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (FunctionOrValue [] "c")
                                                            , Node { start = { row = 1, column = 21 }, end = { row = 1, column = 23 } } (Operator "==")
                                                            , Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (CharLiteral ' ')
                                                            , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 30 } } (Operator "||")
                                                            , Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (FunctionOrValue [] "c")
                                                            , Node { start = { row = 1, column = 33 }, end = { row = 1, column = 35 } } (Operator "==")
                                                            , Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } } (CharLiteral '\n')
                                                            , Node { start = { row = 1, column = 41 }, end = { row = 1, column = 43 } } (Operator "||")
                                                            , Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (FunctionOrValue [] "c")
                                                            , Node { start = { row = 1, column = 46 }, end = { row = 1, column = 48 } } (Operator "==")
                                                            , Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } } (CharLiteral '\u{000D}')
                                                            ]
                                                        )
                                                , args = [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (VarPattern "c") ]
                                                }
                                            )
                                        )
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
