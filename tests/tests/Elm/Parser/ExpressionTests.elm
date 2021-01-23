module Elm.Parser.ExpressionTests exposing (all)

import Combine exposing (whitespace)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
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
        , test "String literal" <|
            \() ->
                parseFullStringWithNullState "\"Bar\"" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (Literal "Bar"))
        , test "character literal" <|
            \() ->
                parseFullStringWithNullState "'c'" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (CharLiteral 'c'))
        , test "tuple expression" <|
            \() ->
                parseFullStringWithNullState "(1,2)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (TupleExpression [ Node emptyRange <| Integer 1, Node emptyRange <| Integer 2 ]))
        , test "prefix expression" <|
            \() ->
                parseFullStringWithNullState "(,)" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (PrefixOperator ","))
        , test "String literal multiline" <|
            \() ->
                parseFullStringWithNullState "\"\"\"Bar foo \n a\"\"\"" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (Literal "Bar foo \n a"))
        , test "Regression test for muliline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\{\\}\"\"\"" expression
                    |> Maybe.map Node.value
                    |> Expect.equal Nothing
        , test "Regression test 2 for muliline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\{\\\\}\"\"\"" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (Literal "\\{\\}"))
        , test "Regression test 3 for muliline strings with backslashes" <|
            \() ->
                parseFullStringWithNullState "\"\"\"\\\\a-blablabla-\\\\b\"\"\"" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (Literal "\\a-blablabla-\\b"))
        , test "Type expression for upper case" <|
            \() ->
                parseFullStringWithNullState "Bar" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [] "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                parseFullStringWithNullState "bar" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [] "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                parseFullStringWithNullState "Bar.foo" expression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [ "Bar" ] "foo"))
        , test "parenthesizedExpression" <|
            \() ->
                parseFullStringWithNullState "(bar)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (TupleExpression
                                [ Node emptyRange <| FunctionOrValue [] "bar" ]
                            )
                        )
        , test "application expression" <|
            \() ->
                parseFullStringWithNullState "List.concat []" expression
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                                Application
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                    [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListExpr []
                                    ]
                            )
                        )
        , test "application expression with operator" <|
            \() ->
                parseFullStringWithNullState "model + 1" expression
                    |> Expect.equal
                        (Just
                            (Node (Range { column = 1, row = 1 } { column = 10, row = 1 }) <|
                                Application
                                    (Node { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } } <| FunctionOrValue [] "model")
                                    [ Node { end = { column = 8, row = 1 }, start = { column = 7, row = 1 } } <| Operator "+"
                                    , Node { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } } <| Integer 1
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
                                    [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } <| Literal ""
                                    , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } } <|
                                        Application
                                            (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } <| FunctionOrValue [] "always")
                                            [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } } <|
                                                TupleExpression
                                                    [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } } <|
                                                        Application
                                                            (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } <|
                                                                FunctionOrValue [ "List" ] "concat"
                                                            )
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
                                                    ]
                                            ]
                                    ]
                            )
                        )
        , test "expressionNotApplication simple" <|
            \() ->
                parseFullStringWithNullState "foo" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [] "foo"))
        , test "unit application" <|
            \() ->
                parseFullStringWithNullState "Task.succeed ()" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange <|
                                    FunctionOrValue [ "Task" ] "succeed"
                                )
                                [ Node emptyRange <| TupleExpression [] ]
                            )
                        )
        , test "compoundExpression" <|
            \() ->
                parseFullStringWithNullState "foo bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange <| FunctionOrValue [] "foo")
                                [ Node emptyRange <| FunctionOrValue [] "bar" ]
                            )
                        )
        , test "compoundExpression 2" <|
            \() ->
                parseFullStringWithNullState "{ key = value } ! []" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange <| RecordExpr [ Node emptyRange ( Node emptyRange "key", Node emptyRange <| FunctionOrValue [] "value" ) ])
                                [ Node emptyRange <| Operator "!"
                                , Node emptyRange <| ListExpr []
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
                            (IfBlock
                                (Node emptyRange <| FunctionOrValue [] "True")
                                (Node emptyRange <| FunctionOrValue [] "foo")
                                (Node emptyRange <| FunctionOrValue [] "bar")
                            )
                        )
        , test "nestedIfExpression" <|
            \() ->
                parseFullStringWithNullState "if True then if False then foo else baz else bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (IfBlock
                                (Node emptyRange <| FunctionOrValue [] "True")
                                (Node emptyRange <|
                                    IfBlock
                                        (Node emptyRange <| FunctionOrValue [] "False")
                                        (Node emptyRange <| FunctionOrValue [] "foo")
                                        (Node emptyRange <| FunctionOrValue [] "baz")
                                )
                                (Node emptyRange <| FunctionOrValue [] "bar")
                            )
                        )
        , test "recordExpression" <|
            \() ->
                parseFullStringWithNullState "{ model = 0, view = view, update = update }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordExpr
                                [ Node emptyRange ( Node emptyRange "model", Node emptyRange <| Integer 0 )
                                , Node emptyRange ( Node emptyRange "view", Node emptyRange <| FunctionOrValue [] "view" )
                                , Node emptyRange ( Node emptyRange "update", Node emptyRange <| FunctionOrValue [] "update" )
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
                            (RecordExpr
                                [ Node emptyRange ( Node emptyRange "foo", Node emptyRange <| Integer 1 )
                                , Node emptyRange ( Node emptyRange "baz", Node emptyRange <| Integer 2 )
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
                            (ListExpr
                                [ Node emptyRange <| Application (Node emptyRange <| FunctionOrValue [] "class") [ Node emptyRange <| Literal "a" ]
                                , Node emptyRange <| Application (Node emptyRange <| FunctionOrValue [] "text") [ Node emptyRange <| Literal "Foo" ]
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
                            (ListExpr
                                [ Node emptyRange <| Integer 1
                                ]
                            )
                        )
        , test "listExpression empty with comment" <|
            \() ->
                parseFullStringWithNullState "[{-| Foo -}]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (ListExpr []))
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
                                (Node emptyRange <|
                                    RecordAccess
                                        (Node emptyRange <| FunctionOrValue [] "foo")
                                        (Node emptyRange "bar")
                                )
                                (Node emptyRange "baz")
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
                                (Node emptyRange <|
                                    RecordAccess
                                        (Node emptyRange <| FunctionOrValue [ "A", "B" ] "foo")
                                        (Node emptyRange "bar")
                                )
                                (Node emptyRange "baz")
                            )
                        )
        , test "record update" <|
            \() ->
                parseFullStringWithNullState "{ model | count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordUpdateExpression
                                (Node emptyRange "model")
                                (Node emptyRange ( Node emptyRange "count", Node emptyRange <| Integer 1 ))
                                [ Node emptyRange ( Node emptyRange "loading", Node emptyRange <| FunctionOrValue [] "True" ) ]
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                parseFullStringWithNullState "{model| count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (RecordUpdateExpression
                                (Node emptyRange "model")
                                (Node emptyRange ( Node emptyRange "count", Node emptyRange <| Integer 1 ))
                                [ Node emptyRange ( Node emptyRange "loading", Node emptyRange <| FunctionOrValue [] "True" ) ]
                            )
                        )
        , test "record access as function" <|
            \() ->
                parseFullStringWithNullState "List.map .name people" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange <| FunctionOrValue [ "List" ] "map")
                                [ Node emptyRange <| RecordAccessFunction ".name"
                                , Node emptyRange <| FunctionOrValue [] "people"
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
                                [ Node emptyRange <|
                                    Application
                                        (Node emptyRange <| RecordAccessFunction ".spaceEvenly")
                                        [ Node emptyRange <| FunctionOrValue [ "Internal", "Style" ] "classes" ]
                                ]
                            )
                        )
        , test "positive integer should be invalid" <|
            \() ->
                parseFullStringWithNullState "+1" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal Nothing
        , test "expression ending with an operator should not be valid" <|
            \() ->
                parseFullStringWithNullState "1++" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal Nothing
        , test "prefix notation" <|
            \() ->
                parseFullStringWithNullState "(::) x" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just <|
                            Application
                                (Node emptyRange <| PrefixOperator "::")
                                [ Node emptyRange <| FunctionOrValue [] "x" ]
                        )
        , test "negated expression for value" <|
            \() ->
                parseFullStringWithNullState "-x" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (Negation (Node emptyRange <| FunctionOrValue [] "x")))
        , test "negated expression in application" <|
            \() ->
                parseFullStringWithNullState "toFloat -5" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange <| FunctionOrValue [] "toFloat")
                                [ Node emptyRange <| Negation (Node emptyRange <| Integer 5) ]
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
                                (Node emptyRange <|
                                    TupleExpression
                                        [ Node emptyRange <|
                                            Application
                                                (Node emptyRange <| FunctionOrValue [] "x")
                                                [ Node emptyRange <| Operator "-"
                                                , Node emptyRange <| FunctionOrValue [] "y"
                                                ]
                                        ]
                                )
                            )
                        )
        , test "function with higher order" <|
            \() ->
                parseFullStringWithNullState "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (Application
                                (Node emptyRange (FunctionOrValue [] "chompWhile"))
                                [ Node emptyRange
                                    (TupleExpression
                                        [ Node emptyRange
                                            (LambdaExpression
                                                { firstArg = Node emptyRange (VarPattern_ "c")
                                                , restOfArgs = []
                                                , expression =
                                                    Node emptyRange
                                                        (Application
                                                            (Node emptyRange (FunctionOrValue [] "c"))
                                                            [ Node emptyRange (Operator "==")
                                                            , Node emptyRange (CharLiteral ' ')
                                                            , Node emptyRange (Operator "||")
                                                            , Node emptyRange (FunctionOrValue [] "c")
                                                            , Node emptyRange (Operator "==")
                                                            , Node emptyRange (CharLiteral '\n')
                                                            , Node emptyRange (Operator "||")
                                                            , Node emptyRange (FunctionOrValue [] "c")
                                                            , Node emptyRange (Operator "==")
                                                            , Node emptyRange (CharLiteral '\u{000D}')
                                                            ]
                                                        )
                                                }
                                            )
                                        ]
                                    )
                                ]
                            )
                        )
        ]
