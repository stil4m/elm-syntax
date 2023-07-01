module Elm.Parser.ExpressionV2Tests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.ExpressionV2 exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Parser
import Test exposing (..)


parseExpression : String -> Maybe (Node Expression)
parseExpression source =
    Parser.run expression source
        |> Result.toMaybe


all : Test
all =
    describe "ExpressionTests"
        [ test "empty" <|
            \() ->
                parseExpression ""
                    |> Expect.equal Nothing
        , test "Integer literal" <|
            \() ->
                parseExpression "101"
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (IntegerLiteral 101))
        , Test.skip <|
            test "String literal" <|
                \() ->
                    parseExpression "\"Bar\""
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (StringLiteral SingleQuote "Bar"))
        , Test.skip <|
            test "character literal" <|
                \() ->
                    parseExpression "'c'"
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (CharLiteral 'c'))
        , Test.skip <|
            test "tuple expression" <|
                \() ->
                    parseExpression "(1,2)"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (TupleExpression [ Node.empty <| IntegerLiteral 1, Node.empty <| IntegerLiteral 2 ]))
        , Test.skip <|
            test "prefix expression" <|
                \() ->
                    parseExpression "(,)"
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (PrefixOperator ","))
        , Test.skip <|
            test "String literal multiline" <|
                \() ->
                    parseExpression "\"\"\"Bar foo \n a\"\"\""
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (StringLiteral TripleQuote "Bar foo \n a"))
        , Test.skip <|
            test "Regression test for multiline strings with backslashes" <|
                \() ->
                    parseExpression "\"\"\"\\{\\}\"\"\""
                        |> Maybe.map Node.value
                        |> Expect.equal Nothing
        , Test.skip <|
            test "Regression test 2 for multiline strings with backslashes" <|
                \() ->
                    parseExpression "\"\"\"\\\\{\\\\}\"\"\""
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (StringLiteral TripleQuote "\\{\\}"))
        , Test.skip <|
            test "Regression test 3 for multiline strings with backslashes" <|
                \() ->
                    parseExpression "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (StringLiteral TripleQuote "\\a-blablabla-\\b"))
        , Test.skip <|
            test "Type expression for upper case" <|
                \() ->
                    parseExpression "Bar"
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (FunctionOrValue [] "Bar"))
        , Test.skip <|
            test "Type expression for lower case" <|
                \() ->
                    parseExpression "bar"
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (FunctionOrValue [] "bar"))
        , Test.skip <|
            test "Type expression for lower case but qualified" <|
                \() ->
                    parseExpression "Bar.foo"
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (FunctionOrValue [ "Bar" ] "foo"))
        , Test.skip <|
            test "parenthesizedExpression" <|
                \() ->
                    parseExpression "(bar)"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (TupleExpression
                                    [ Node.empty <| FunctionOrValue [] "bar" ]
                                )
                            )
        , Test.skip <|
            test "application expression" <|
                \() ->
                    parseExpression "List.concat []"
                        |> Expect.equal
                            (Just
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                                    FunctionCall
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                        [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListLiteral []
                                        ]
                                )
                            )
        , Test.skip <|
            test "application expression with operator" <|
                \() ->
                    parseExpression "model + 1"
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
        , Test.skip <|
            test "application expression 2" <|
                \() ->
                    parseExpression "(\"\", always (List.concat [ [ fileName ], [] ]))"
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
        , Test.skip <|
            test "expressionNotApplication simple" <|
                \() ->
                    parseExpression "foo"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (FunctionOrValue [] "foo"))
        , Test.skip <|
            test "unit application" <|
                \() ->
                    parseExpression "Task.succeed ()"
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
        , Test.skip <|
            test "compoundExpression" <|
                \() ->
                    parseExpression "foo bar"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (FunctionCall
                                    (Node.empty <| FunctionOrValue [] "foo")
                                    [ Node.empty <| FunctionOrValue [] "bar" ]
                                )
                            )
        , Test.skip <|
            test "compoundExpression 2" <|
                \() ->
                    parseExpression "{ key = value } ! []"
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
        , Test.skip <|
            test "ifBlockExpression" <|
                \() ->
                    parseExpression "if True then foo else bar"
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
        , Test.skip <|
            test "nestedIfExpression" <|
                \() ->
                    parseExpression "if True then if False then foo else baz else bar"
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
        , Test.skip <|
            test "recordExpression" <|
                \() ->
                    parseExpression "{ model = 0, view = view, update = update }"
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
        , Test.skip <|
            test "recordExpression with comment" <|
                \() ->
                    parseExpression "{ foo = 1 -- bar\n , baz = 2 }"
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
        , Test.skip <|
            test "listExpression" <|
                \() ->
                    parseExpression "[ class \"a\", text \"Foo\"]"
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
        , Test.skip <|
            test "listExpression singleton with comment" <|
                \() ->
                    parseExpression "[ 1 {- Foo-} ]"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (ListLiteral
                                    [ Node empty <| IntegerLiteral 1
                                    ]
                                )
                            )
        , Test.skip <|
            test "listExpression empty with comment" <|
                \() ->
                    parseExpression "[{-| Foo -}]"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (ListLiteral []))
        , Test.skip <|
            test "qualified expression" <|
                \() ->
                    parseExpression "Html.text"
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (FunctionOrValue [ "Html" ] "text")
                            )
        , Test.skip <|
            test "record access" <|
                \() ->
                    parseExpression "foo.bar"
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
        , Test.skip <|
            test "multiple record access operations" <|
                \() ->
                    parseExpression "foo.bar.baz"
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
        , Test.skip <|
            test "multiple record access operations with module name" <|
                \() ->
                    parseExpression "A.B.foo.bar.baz"
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
        , Test.skip <|
            test "record update" <|
                \() ->
                    parseExpression "{ model | count = 1, loading = True }"
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
        , Test.skip <|
            test "record update no spacing" <|
                \() ->
                    parseExpression "{model| count = 1, loading = True }"
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
        , Test.skip <|
            test "record access as function" <|
                \() ->
                    parseExpression "List.map .name people"
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
        , Test.skip <|
            test "record access direct" <|
                \() ->
                    parseExpression "(.spaceEvenly Internal.Style.classes)"
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
        , Test.skip <|
            test "positive integer should be invalid" <|
                \() ->
                    parseExpression "+1"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal Nothing
        , Test.skip <|
            test "expression ending with an operator should not be valid" <|
                \() ->
                    parseExpression "1++"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal Nothing
        , Test.skip <|
            test "prefix notation" <|
                \() ->
                    parseExpression "(::) x"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just <|
                                FunctionCall
                                    (Node empty <| PrefixOperator "::")
                                    [ Node empty <| FunctionOrValue [] "x" ]
                            )
        , Test.skip <|
            test "negated expression for value" <|
                \() ->
                    parseExpression "-x"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal (Just (Negation (Node empty <| FunctionOrValue [] "x")))
        , Test.skip <|
            test "negated expression in application" <|
                \() ->
                    parseExpression "toFloat -5"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (FunctionCall
                                    (Node empty <| FunctionOrValue [] "toFloat")
                                    [ Node empty <| Negation (Node empty <| IntegerLiteral 5) ]
                                )
                            )
        , Test.skip <|
            test "negated expression for parenthesized" <|
                \() ->
                    parseExpression "-(x - y)"
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
        , Test.skip <|
            test "function with higher order" <|
                \() ->
                    parseExpression "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> Maybe.map noRangeExpression
                        |> Maybe.map Node.value
                        |> Expect.equal
                            (Just
                                (FunctionCall
                                    (Node empty (FunctionOrValue [] "chompWhile"))
                                    [ Node empty
                                        (TupleExpression
                                            [ Node empty
                                                (LambdaExpression
                                                    { firstArg = Node empty (VarPattern_ "c")
                                                    , restOfArgs = []
                                                    , expression =
                                                        Node empty
                                                            (FunctionCall
                                                                (Node empty (FunctionOrValue [] "c"))
                                                                [ Node empty (Operator "==")
                                                                , Node empty (CharLiteral ' ')
                                                                , Node empty (Operator "||")
                                                                , Node empty (FunctionOrValue [] "c")
                                                                , Node empty (Operator "==")
                                                                , Node empty (CharLiteral '\n')
                                                                , Node empty (Operator "||")
                                                                , Node empty (FunctionOrValue [] "c")
                                                                , Node empty (Operator "==")
                                                                , Node empty (CharLiteral '\u{000D}')
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
