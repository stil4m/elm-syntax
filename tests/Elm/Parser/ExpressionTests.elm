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
                    |> Expect.equal
                        (Just
                            (Node { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } }
                                (TupleExpression
                                    [ Node { end = { column = 3, row = 1 }, start = { column = 2, row = 1 } } (IntegerLiteral 1)
                                    , Node { end = { column = 5, row = 1 }, start = { column = 4, row = 1 } } (IntegerLiteral 2)
                                    ]
                                )
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
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (TupleExpression
                                [ Node.empty <| FunctionOrValue [] "bar" ]
                            )
                        )
        , test "application expression" <|
            \() ->
                parseFullStringWithNullState "List.concat []" expression
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                                FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                    [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListLiteral []
                                    ]
                            )
                        )
        , test "application expression with operator" <|
            \() ->
                parseFullStringWithNullState "model + 1" expression
                    |> Expect.equal
                        (Just
                            (Node (Range { column = 1, row = 1 } { column = 10, row = 1 }) <|
                                FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } <| FunctionOrValue [] "model")
                                    [ Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } <| Operator "+"
                                    , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| IntegerLiteral 1
                                    ]
                            )
                        )
        , test "application expression 2" <|
            \() ->
                parseFullStringWithNullState "(\"\", always (List.concat [ [ fileName ], [] ]))" expression
                    |> Expect.equal
                        (Just
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
                        )
        , test "expressionNotApplication simple" <|
            \() ->
                parseFullStringWithNullState "foo" expression
                    |> expectAst (Node { end = { column = 4, row = 1 }, start = { column = 1, row = 1 } } (FunctionOrValue [] "foo"))
        , test "unit application" <|
            \() ->
                parseFullStringWithNullState "Task.succeed ()" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionCall
                                (Node.empty <|
                                    FunctionOrValue [ "Task" ] "succeed"
                                )
                                [ Node.empty <| TupleExpression [] ]
                            )
                        )
        , test "compoundExpression" <|
            \() ->
                parseFullStringWithNullState "foo bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionCall
                                (Node.empty <| FunctionOrValue [] "foo")
                                [ Node.empty <| FunctionOrValue [] "bar" ]
                            )
                        )
        , test "compoundExpression 2" <|
            \() ->
                parseFullStringWithNullState "{ key = value } ! []" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionCall
                                (Node empty <| Record [ Node empty ( Node empty "key", Node empty <| FunctionOrValue [] "value" ) ])
                                [ Node empty <| Operator "!"
                                , Node empty <| ListLiteral []
                                ]
                            )
                        )
        , test "ifBlockExpression" <|
            \() ->
                parseFullStringWithNullState "if True then foo else bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (If
                                (Node empty <| FunctionOrValue [] "True")
                                (Node empty <| FunctionOrValue [] "foo")
                                (Node empty <| FunctionOrValue [] "bar")
                            )
                        )
        , test "nestedIfExpression" <|
            \() ->
                parseFullStringWithNullState "if True then if False then foo else baz else bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (If
                                (Node empty <| FunctionOrValue [] "True")
                                (Node empty <|
                                    If
                                        (Node empty <| FunctionOrValue [] "False")
                                        (Node empty <| FunctionOrValue [] "foo")
                                        (Node empty <| FunctionOrValue [] "baz")
                                )
                                (Node empty <| FunctionOrValue [] "bar")
                            )
                        )
        , test "recordExpression" <|
            \() ->
                parseFullStringWithNullState "{ model = 0, view = view, update = update }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Record
                                [ Node empty ( Node empty "model", Node empty <| IntegerLiteral 0 )
                                , Node empty ( Node empty "view", Node empty <| FunctionOrValue [] "view" )
                                , Node empty ( Node empty "update", Node empty <| FunctionOrValue [] "update" )
                                ]
                            )
                        )
        , test "recordExpression with comment" <|
            \() ->
                parseFullStringWithNullState "{ foo = 1 -- bar\n , baz = 2 }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Record
                                [ Node empty ( Node empty "foo", Node empty <| IntegerLiteral 1 )
                                , Node empty ( Node empty "baz", Node empty <| IntegerLiteral 2 )
                                ]
                            )
                        )
        , test "listExpression" <|
            \() ->
                parseFullStringWithNullState "[ class \"a\", text \"Foo\"]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (ListLiteral
                                [ Node empty <| FunctionCall (Node empty <| FunctionOrValue [] "class") [ Node empty <| StringLiteral SingleQuote "a" ]
                                , Node empty <| FunctionCall (Node empty <| FunctionOrValue [] "text") [ Node empty <| StringLiteral SingleQuote "Foo" ]
                                ]
                            )
                        )
        , test "listExpression singleton with comment" <|
            \() ->
                parseFullStringWithNullState "[ 1 {- Foo-} ]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (ListLiteral
                                [ Node empty <| IntegerLiteral 1
                                ]
                            )
                        )
        , test "listExpression empty with comment" <|
            \() ->
                parseFullStringWithNullState "[{-| Foo -}]" expression
                    |> Maybe.map noRangeExpression
                    |> expectAst (Node.empty (ListLiteral []))
        , test "qualified expression" <|
            \() ->
                parseFullStringWithNullState "Html.text" expression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionOrValue [ "Html" ] "text")
                        )
        , test "record access" <|
            \() ->
                parseFullStringWithNullState "foo.bar" expression
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } <|
                                RecordAccess
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } <|
                                        FunctionOrValue [] "foo"
                                    )
                                    (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                            )
                        )
        , test "multiple record access operations" <|
            \() ->
                parseFullStringWithNullState "foo.bar.baz" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordAccess
                                (Node empty <|
                                    RecordAccess
                                        (Node empty <| FunctionOrValue [] "foo")
                                        (Node empty "bar")
                                )
                                (Node empty "baz")
                            )
                        )
        , test "multiple record access operations with module name" <|
            \() ->
                parseFullStringWithNullState "A.B.foo.bar.baz" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordAccess
                                (Node empty <|
                                    RecordAccess
                                        (Node empty <| FunctionOrValue [ "A", "B" ] "foo")
                                        (Node empty "bar")
                                )
                                (Node empty "baz")
                            )
                        )
        , test "record update" <|
            \() ->
                parseFullStringWithNullState "{ model | count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordUpdate
                                (Node empty "model")
                                (Node empty ( Node empty "count", Node empty <| IntegerLiteral 1 ))
                                [ Node empty ( Node empty "loading", Node empty <| FunctionOrValue [] "True" ) ]
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                parseFullStringWithNullState "{model| count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordUpdate
                                (Node empty "model")
                                (Node empty ( Node empty "count", Node empty <| IntegerLiteral 1 ))
                                [ Node empty ( Node empty "loading", Node empty <| FunctionOrValue [] "True" ) ]
                            )
                        )
        , test "record access as function" <|
            \() ->
                parseFullStringWithNullState "List.map .name people" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionCall
                                (Node empty <| FunctionOrValue [ "List" ] "map")
                                [ Node empty <| RecordAccessFunction "name"
                                , Node empty <| FunctionOrValue [] "people"
                                ]
                            )
                        )
        , test "record access direct" <|
            \() ->
                parseFullStringWithNullState "(.spaceEvenly Internal.Style.classes)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (TupleExpression
                                [ Node empty <|
                                    FunctionCall
                                        (Node empty <| RecordAccessFunction "spaceEvenly")
                                        [ Node empty <| FunctionOrValue [ "Internal", "Style" ] "classes" ]
                                ]
                            )
                        )
        , test "positive integer should be invalid" <|
            \() ->
                parseFullStringWithNullState "+1" expression
                    |> Maybe.map noRangeExpression
                    |> Expect.equal Nothing
        , test "expression ending with an operator should not be valid" <|
            \() ->
                parseFullStringWithNullState "1++" expression
                    |> Maybe.map noRangeExpression
                    |> Expect.equal Nothing
        , test "prefix notation" <|
            \() ->
                parseFullStringWithNullState "(::) x" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just <|
                            FunctionCall
                                (Node empty <| PrefixOperator "::")
                                [ Node empty <| FunctionOrValue [] "x" ]
                        )
        , test "negated expression for value" <|
            \() ->
                parseFullStringWithNullState "-x" expression
                    |> Maybe.map noRangeExpression
                    |> expectAst (Node.empty (Negation (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "x"))))
        , test "negated expression in application" <|
            \() ->
                parseFullStringWithNullState "toFloat -5" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (FunctionCall
                                (Node empty <| FunctionOrValue [] "toFloat")
                                [ Node empty <| Negation (Node empty <| IntegerLiteral 5) ]
                            )
                        )
        , test "negated expression for parenthesized" <|
            \() ->
                parseFullStringWithNullState "-(x - y)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Negation
                                (Node empty <|
                                    TupleExpression
                                        [ Node empty <|
                                            FunctionCall
                                                (Node empty <| FunctionOrValue [] "x")
                                                [ Node empty <| Operator "-"
                                                , Node empty <| FunctionOrValue [] "y"
                                                ]
                                        ]
                                )
                            )
                        )
        , test "function with higher order" <|
            \() ->
                parseFullStringWithNullState "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')" expression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
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
