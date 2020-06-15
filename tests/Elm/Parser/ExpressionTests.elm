module Elm.Parser.ExpressionTests exposing (all)

import Elm.Parser.Expression exposing (expression)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ExpressionTests"
        [ test "empty" <|
            \() ->
                ""
                    |> expectInvalid
        , test "Integer literal" <|
            \() ->
                "101"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (Integer 101))
        , test "String literal" <|
            \() ->
                "\"Bar\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (Literal "Bar"))
        , test "character literal" <|
            \() ->
                "'c'"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (CharLiteral 'c'))
        , test "tuple expression" <|
            \() ->
                "(1,2)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (TupledExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Integer 1)
                                , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (Integer 2)
                                ]
                            )
                        )
        , test "tuple expression with spaces" <|
            \() ->
                "( 1  ,  2 )"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (TupledExpression
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Integer 1)
                                , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (Integer 2)
                                ]
                            )
                        )
        , test "String literal multiline" <|
            \() ->
                "\"\"\"Bar foo \n a\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (Literal "Bar foo \n a"))
        , test "Regression test for multiline strings with backslashes" <|
            \() ->
                "\"\"\"\\{\\}\"\"\""
                    |> expectInvalid
        , test "Regression test 2 for multiline strings with backslashes" <|
            \() ->
                "\"\"\"\\\\{\\\\}\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (Literal "\\{\\}"))
        , test "Regression test 3 for multiline strings with backslashes" <|
            \() ->
                "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (Literal "\\a-blablabla-\\b"))
        , test "Type expression for upper case" <|
            \() ->
                "Bar"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                "bar"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                "Bar.foo"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [ "Bar" ] "foo"))
        , test "parenthesizedExpression" <|
            \() ->
                "(bar)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (ParenthesizedExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "parenthesized expression starting with a negation" <|
            \() ->
                "(-1 * sign)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                            (ParenthesizedExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                                    (OperatorApplication "*"
                                        Left
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }
                                            (Negation (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Integer 1)))
                                        )
                                        (Node { start = { row = 1, column = 7 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "sign"))
                                    )
                                )
                            )
                        )
        , test "application expression" <|
            \() ->
                "List.concat []"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                            Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListExpr []
                                ]
                        )
        , test "application expression with operator" <|
            \() ->
                "model + 1"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } <|
                            OperatorApplication "+"
                                Infix.Left
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } <| FunctionOrValue [] "model")
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| Integer 1)
                        )
        , test "application expression 2" <|
            \() ->
                "(\"\", always (List.concat [ [ fileName ], [] ]))"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } } <|
                            TupledExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } <| Literal ""
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } } <|
                                    Application (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } <| FunctionOrValue [] "always")
                                        [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } } <|
                                            ParenthesizedExpression
                                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } } <|
                                                    Application (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } (FunctionOrValue [ "List" ] "concat"))
                                                        [ Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } } <|
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
                "foo"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
        , test "unit application" <|
            \() ->
                "Task.succeed ()"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (FunctionOrValue [ "Task" ] "succeed"))
                                [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } UnitExpr
                                ]
                            )
                        )
        , test "Function call" <|
            \() ->
                "foo bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "bar")
                                ]
                            )
                        )
        , test "Function call with argument badly indented" <|
            \() ->
                "foo\nbar"
                    |> expectInvalid
        , test "ifBlockExpression" <|
            \() ->
                "if True then foo else bar"
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
                "if True then if False then foo else baz else bar"
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
                "{ model = 0, view = view, update = update }"
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
                "{ foo = 1 -- bar\n , baz = 2 }"
                    |> expectAstWithComments
                        { ast =
                            Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
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
                        , comments = [ Node { start = { row = 1, column = 11 }, end = { row = 1, column = 17 } } "-- bar" ]
                        }
        , test "listExpression" <|
            \() ->
                "[ class \"a\", text \"Foo\"]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            (ListExpr
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    (Application (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "class"))
                                        [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } (Literal "a")
                                        ]
                                    )
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 24 } }
                                    (Application (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } (FunctionOrValue [] "text"))
                                        [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } (Literal "Foo")
                                        ]
                                    )
                                ]
                            )
                        )
        , test "listExpression singleton with comment" <|
            \() ->
                "[ 1 {- Foo-} ]"
                    |> expectAstWithComments
                        { ast =
                            Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (ListExpr
                                    [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Integer 1)
                                    ]
                                )
                        , comments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 13 } } "{- Foo-}" ]
                        }
        , test "listExpression empty with comment" <|
            \() ->
                "[{- Foo -}]"
                    |> expectAstWithComments
                        { ast = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (ListExpr [])
                        , comments = [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } } "{- Foo -}" ]
                        }
        , test "qualified expression" <|
            \() ->
                "Html.text"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (FunctionOrValue [ "Html" ] "text"))
        , test "record access" <|
            \() ->
                "foo.bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                            (RecordAccess
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                            )
                        )
        , test "multiple record access operations" <|
            \() ->
                "foo.bar.baz"
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
                "A.B.foo.bar.baz"
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
                "{ model | count = 1, loading = True }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (RecordUpdateExpression
                                (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                    ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                    , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (Integer 1)
                                    )
                                )
                                [ Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                    ( Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                    , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                "{model| count = 1, loading = True }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            (RecordUpdateExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                    ( Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                    , Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Integer 1)
                                    )
                                )
                                [ Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                    ( Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                    , Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "record access as function" <|
            \() ->
                "List.map .name people"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (FunctionOrValue [ "List" ] "map"))
                                [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (RecordAccessFunction ".name")
                                , Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (FunctionOrValue [] "people")
                                ]
                            )
                        )
        , test "record access direct" <|
            \() ->
                "(.spaceEvenly Internal.Style.classes)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (ParenthesizedExpression
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                    (Application
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (RecordAccessFunction ".spaceEvenly"))
                                        [ Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (FunctionOrValue [ "Internal", "Style" ] "classes")
                                        ]
                                    )
                                )
                            )
                        )
        , test "positive integer should be invalid" <|
            \() ->
                "+1"
                    |> expectInvalid
        , test "expression ending with an operator should not be valid" <|
            \() ->
                "1++"
                    |> expectInvalid
        , test "prefix notation" <|
            \() ->
                "(::) x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (PrefixOperator "::"))
                                [ Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (FunctionOrValue [] "x")
                                ]
                            )
                        )
        , test "subtraction without spaces" <|
            \() ->
                "2-1"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } }
                            (OperatorApplication "-"
                                Left
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Integer 2))
                                (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (Integer 1))
                            )
                        )
        , test "negated expression for value" <|
            \() ->
                "-x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            (Negation (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (FunctionOrValue [] "x")))
                        )
        , test "negated expression in application" <|
            \() ->
                "toFloat -5"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "toFloat"))
                                [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                    (Negation (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (Integer 5)))
                                ]
                            )
                        )
        , test "negated expression for parenthesized" <|
            \() ->
                "-(x - y)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (Negation
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 9 } }
                                    (ParenthesizedExpression
                                        (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                            (OperatorApplication "-"
                                                Left
                                                (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "x"))
                                                (Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "y"))
                                            )
                                        )
                                    )
                                )
                            )
                        )
        , test "negated expression with other operations" <|
            \() ->
                "-1 + -10 * -100^2 == -100001"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 29 } }
                            (OperatorApplication "=="
                                Non
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 18 } }
                                    (OperatorApplication "+"
                                        Left
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                            (Negation (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Integer 1)))
                                        )
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 18 } }
                                            (OperatorApplication "*"
                                                Left
                                                (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 9 } }
                                                    (Negation (Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Integer 10)))
                                                )
                                                (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 18 } }
                                                    (OperatorApplication "^"
                                                        Right
                                                        (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 16 } }
                                                            (Negation
                                                                (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } (Integer 100))
                                                            )
                                                        )
                                                        (Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (Integer 2))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } }
                                    (Negation (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 29 } } (Integer 100001)))
                                )
                            )
                        )
        , test "plus and minus in the same expression" <|
            \() ->
                " 1 + 2 - 3"
                    |> expectAst
                        (Node
                            { start = { row = 1, column = 2 }, end = { row = 1, column = 11 } }
                            (OperatorApplication "-"
                                Left
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                    (OperatorApplication "+"
                                        Left
                                        (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (Integer 1))
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (Integer 2))
                                    )
                                )
                                (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (Integer 3))
                            )
                        )
        , test "pipe operation" <|
            \() ->
                "a |> b"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                            (OperatorApplication "|>"
                                Left
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (FunctionOrValue [] "a"))
                                (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (FunctionOrValue [] "b"))
                            )
                        )
        , test "function with higher order" <|
            \() ->
                "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "chompWhile"))
                                [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                    (ParenthesizedExpression
                                        (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                            (LambdaExpression
                                                { args = [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (VarPattern "c") ]
                                                , expression =
                                                    Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                        (OperatorApplication "||"
                                                            Right
                                                            (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 27 } }
                                                                (OperatorApplication "=="
                                                                    Non
                                                                    (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (FunctionOrValue [] "c"))
                                                                    (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (CharLiteral ' '))
                                                                )
                                                            )
                                                            (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 53 } }
                                                                (OperatorApplication "||"
                                                                    Right
                                                                    (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 40 } }
                                                                        (OperatorApplication "=="
                                                                            Non
                                                                            (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (FunctionOrValue [] "c"))
                                                                            (Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } }
                                                                                (CharLiteral '\n')
                                                                            )
                                                                        )
                                                                    )
                                                                    (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 53 } }
                                                                        (OperatorApplication "=="
                                                                            Non
                                                                            (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (FunctionOrValue [] "c"))
                                                                            (Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } }
                                                                                (CharLiteral '\u{000D}')
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                }
                                            )
                                        )
                                    )
                                ]
                            )
                        )
        , test "application should be lower-priority than field access" <|
            \() ->
                "foo { d | b = f x y }.b"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } }
                            (Application (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 24 } }
                                    (RecordAccess
                                        (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 22 } }
                                            (RecordUpdateExpression (Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "d")
                                                (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 21 } }
                                                    ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } "b"
                                                    , Node { start = { row = 1, column = 15 }, end = { row = 1, column = 20 } }
                                                        (Application (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (FunctionOrValue [] "f"))
                                                            [ Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (FunctionOrValue [] "x")
                                                            , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (FunctionOrValue [] "y")
                                                            ]
                                                        )
                                                    )
                                                )
                                                []
                                            )
                                        )
                                        (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 24 } } "b")
                                    )
                                ]
                            )
                        )
        , test "should not consider a negative number parameter as the start of a new application" <|
            \() ->
                "Random.list -1 generator"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 25 } }
                            (Application
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } (FunctionOrValue [ "Random" ] "list"))
                                [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }
                                    (Negation
                                        (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                            (Integer 1)
                                        )
                                    )
                                , Node { start = { row = 1, column = 16 }, end = { row = 1, column = 25 } } (FunctionOrValue [] "generator")
                                ]
                            )
                        )
        , test "negation can be applied on record access" <|
            \() ->
                "1 + -{x = 10}.x"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (OperatorApplication "+"
                                Left
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 2 } } (Integer 1))
                                (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 16 } }
                                    (Negation
                                        (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 16 } }
                                            (RecordAccess
                                                (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 14 } }
                                                    (RecordExpr
                                                        [ Node { start = { row = 1, column = 7 }, end = { row = 1, column = 13 } }
                                                            ( Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } "x"
                                                            , Node { start = { row = 1, column = 11 }, end = { row = 1, column = 13 } } (Integer 10)
                                                            )
                                                        ]
                                                    )
                                                )
                                                (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } "x")
                                            )
                                        )
                                    )
                                )
                            )
                        )
        , test "fail if condition not positively indented" <|
            \() ->
                """let
    x =
        if
    f y then  1 else 0
in
x"""
                    |> expectInvalid
        , test "fail if `then` not positively indented" <|
            \() ->
                """let
    x =
        if True
   then  1 else 0
in
x"""
                    |> expectInvalid
        , test "fail if if-true-branch not positively indented" <|
            \() ->
                """let
    x =
        if True then
    1   else 0
in
x"""
                    |> expectInvalid
        , test "fail if `else` not positively indented" <|
            \() ->
                """let
    x =
        if True then 1
   else 0
in
x"""
                    |> expectInvalid
        , test "fail if if-false-branch not positively indented" <|
            \() ->
                """let
    x =
        if True then 1 else
    0
in
x"""
                    |> expectInvalid
        , test "fail if record closing curly not positively indented" <|
            \() ->
                """let
    x =
        { a = 0, b = 1
    }
in
x"""
                    |> expectInvalid
        , test "fail if record field value not positively indented" <|
            \() ->
                """let
    x =
        { a = 0, b =
    1 }
in
x"""
                    |> expectInvalid
        , test "fail if record field name not positively indented" <|
            \() ->
                """let
    x =
        { a = 0,
    b       = 1 }
in
x"""
                    |> expectInvalid
        , test "fail if record field `=` not positively indented" <|
            \() ->
                """let
    x =
        { a = 0, b
    =         1 }
in
x"""
                    |> expectInvalid
        , test "fail if tuple closing parens not positively indented" <|
            \() ->
                """let
    x =
        ( 0, 1
    )
in
x"""
                    |> expectInvalid
        , test "fail if tuple part not positively indented" <|
            \() ->
                """let
    x =
        ( 0,
    1   )
in
x"""
                    |> expectInvalid
        , test "fail if operator not positively indented" <|
            \() ->
                """let
    x =
        0
    + 1
in
x"""
                    |> expectInvalid
        , test "fail if function call argument not positively indented" <|
            \() ->
                """let
    x =
        f 0
    1
in
x"""
                    |> expectInvalid
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst expression


expectAstWithComments : { ast : Node Expression, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    ParserWithCommentsUtil.expectAstWithComments expression


expectInvalid : String -> Expect.Expectation
expectInvalid =
    ParserWithCommentsUtil.expectInvalid expression
