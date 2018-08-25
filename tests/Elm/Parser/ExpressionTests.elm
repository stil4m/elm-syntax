module Elm.Parser.ExpressionTests exposing (all, main)

import Combine exposing (whitespace)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


main =
    Tuple.second all


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
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (Literal "Bar"))
        , test "character literal" <|
            \() ->
                parseFullStringWithNullState "'c'" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (CharLiteral 'c'))
        , test "tuple expression" <|
            \() ->
                parseFullStringWithNullState "(1,2)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (TupledExpression [ emptyRanged <| Integer 1, emptyRanged <| Integer 2 ]))
        , test "prefix expression" <|
            \() ->
                parseFullStringWithNullState "(,)" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (PrefixOperator ","))
        , test "String literal multiline" <|
            \() ->
                parseFullStringWithNullState "\"\"\"Bar foo \n a\"\"\"" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (Literal "Bar foo \n a"))
        , test "Type expression for upper case" <|
            \() ->
                parseFullStringWithNullState "Bar" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (FunctionOrValue "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                parseFullStringWithNullState "bar" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (FunctionOrValue "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                parseFullStringWithNullState "Bar.foo" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (QualifiedExpr [ "Bar" ] "foo"))
        , test "operator" <|
            \() ->
                parseFullStringWithNullState "++" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (Operator "++"))
        , test "parenthesizedExpression" <|
            \() ->
                parseFullStringWithNullState "(bar)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (ParenthesizedExpression
                                (emptyRanged <| FunctionOrValue "bar")
                            )
                        )
        , test "application expression" <|
            \() ->
                parseFullStringWithNullState "List.concat []" expression
                    |> Expect.equal
                        (Just
                            ( { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            , Application
                                [ ( { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }, QualifiedExpr [ "List" ] "concat" )
                                , ( { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } }, ListExpr [] )
                                ]
                            )
                        )
        , test "application expression with operator" <|
            \() ->
                parseFullStringWithNullState "model + 1" expression
                    |> Expect.equal
                        (Just
                            ( { end = { column = 10, row = 1 }
                              , start = { column = 1, row = 1 }
                              }
                            , Application
                                [ ( { end = { column = 6, row = 1 }, start = { column = 1, row = 1 } }, FunctionOrValue "model" )
                                , ( { end = { column = 8, row = 1 }, start = { column = 7, row = 1 } }, Operator "+" )
                                , ( { end = { column = 10, row = 1 }, start = { column = 9, row = 1 } }, Integer 1 )
                                ]
                            )
                        )
        , test "application expression 2" <|
            \() ->
                parseFullStringWithNullState "(\"\", always (List.concat [ [ fileName ], [] ]))" expression
                    |> Expect.equal
                        (Just
                            ( { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } }
                            , TupledExpression
                                [ ( { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } }, Literal "" )
                                , ( { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } }
                                  , Application
                                        [ ( { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } }, FunctionOrValue "always" )
                                        , ( { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } }
                                          , ParenthesizedExpression
                                                ( { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } }
                                                , Application
                                                    [ ( { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                                      , QualifiedExpr [ "List" ] "concat"
                                                      )
                                                    , ( { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } }
                                                      , ListExpr
                                                            [ ( { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } }
                                                              , ListExpr
                                                                    [ ( { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } }
                                                                      , FunctionOrValue "fileName"
                                                                      )
                                                                    ]
                                                              )
                                                            , ( { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } }
                                                              , ListExpr []
                                                              )
                                                            ]
                                                      )
                                                    ]
                                                )
                                          )
                                        ]
                                  )
                                ]
                            )
                        )
        , test "expressionNotApplication simple" <|
            \() ->
                parseFullStringWithNullState "foo" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (FunctionOrValue "foo"))
        , test "unit application" <|
            \() ->
                parseFullStringWithNullState "Task.succeed ()" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Application
                                [ emptyRanged <|
                                    QualifiedExpr [ "Task" ] "succeed"
                                , emptyRanged <| UnitExpr
                                ]
                            )
                        )
        , test "compoundExpression" <|
            \() ->
                parseFullStringWithNullState "foo bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Application
                                [ emptyRanged <| FunctionOrValue "foo"
                                , emptyRanged <| FunctionOrValue "bar"
                                ]
                            )
                        )
        , test "compoundExpression 2" <|
            \() ->
                parseFullStringWithNullState "{ key = value } ! []" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Application
                                [ emptyRanged <| RecordExpr [ ( "key", emptyRanged <| FunctionOrValue "value" ) ]
                                , emptyRanged <| Operator "!"
                                , emptyRanged <| ListExpr []
                                ]
                            )
                        )
        , test "ifBlockExpression" <|
            \() ->
                parseFullStringWithNullState "if True then foo else bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (IfBlock
                                (emptyRanged <| FunctionOrValue "True")
                                (emptyRanged <| FunctionOrValue "foo")
                                (emptyRanged <| FunctionOrValue "bar")
                            )
                        )
        , test "nestedIfExpression" <|
            \() ->
                parseFullStringWithNullState "if True then if False then foo else baz else bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (IfBlock
                                (emptyRanged <| FunctionOrValue "True")
                                (emptyRanged <|
                                    IfBlock
                                        (emptyRanged <| FunctionOrValue "False")
                                        (emptyRanged <| FunctionOrValue "foo")
                                        (emptyRanged <| FunctionOrValue "baz")
                                )
                                (emptyRanged <| FunctionOrValue "bar")
                            )
                        )
        , test "recordExpression" <|
            \() ->
                parseFullStringWithNullState "{ model = 0, view = view, update = update }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (RecordExpr
                                [ ( "model", emptyRanged <| Integer 0 )
                                , ( "view", emptyRanged <| FunctionOrValue "view" )
                                , ( "update", emptyRanged <| FunctionOrValue "update" )
                                ]
                            )
                        )
        , test "recordExpression with comment" <|
            \() ->
                parseFullStringWithNullState "{ foo = 1 -- bar\n , baz = 2 }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (RecordExpr
                                [ ( "foo", emptyRanged <| Integer 1 )
                                , ( "baz", emptyRanged <| Integer 2 )
                                ]
                            )
                        )
        , test "listExpression" <|
            \() ->
                parseFullStringWithNullState "[ class \"a\", text \"Foo\"]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (ListExpr
                                [ emptyRanged <| Application [ emptyRanged <| FunctionOrValue "class", emptyRanged <| Literal "a" ]
                                , emptyRanged <| Application [ emptyRanged <| FunctionOrValue "text", emptyRanged <| Literal "Foo" ]
                                ]
                            )
                        )
        , test "listExpression singleton with comment" <|
            \() ->
                parseFullStringWithNullState "[ 1 {- Foo-} ]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (ListExpr
                                [ ( emptyRange
                                  , Integer 1
                                  )
                                ]
                            )
                        )
        , test "listExpression empty with comment" <|
            \() ->
                parseFullStringWithNullState "[{-| Foo -}]" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (ListExpr []))
        , test "qualified expression" <|
            \() ->
                parseFullStringWithNullState "Html.text" expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (QualifiedExpr [ "Html" ]
                                "text"
                            )
                        )
        , test "record access" <|
            \() ->
                parseFullStringWithNullState "foo.bar" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (RecordAccess ( emptyRange, FunctionOrValue "foo" ) "bar"))
        , test "record access multiple" <|
            \() ->
                parseFullStringWithNullState "foo.bar.baz" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (RecordAccess
                                ( emptyRange
                                , RecordAccess
                                    ( emptyRange
                                    , FunctionOrValue "foo"
                                    )
                                    "bar"
                                )
                                "baz"
                            )
                        )
        , test "record update" <|
            \() ->
                parseFullStringWithNullState "{ model | count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (RecordUpdateExpression
                                { name = "model"
                                , updates =
                                    [ ( "count", emptyRanged <| Integer 1 )
                                    , ( "loading", emptyRanged <| FunctionOrValue "True" )
                                    ]
                                }
                            )
                        )
        , test "record update no spacing" <|
            \() ->
                parseFullStringWithNullState "{model| count = 1, loading = True }" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (RecordUpdateExpression
                                { name = "model"
                                , updates =
                                    [ ( "count", emptyRanged <| Integer 1 )
                                    , ( "loading", emptyRanged <| FunctionOrValue "True" )
                                    ]
                                }
                            )
                        )
        , test "record access as function" <|
            \() ->
                parseFullStringWithNullState "List.map .name people" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Application
                                [ emptyRanged <| QualifiedExpr [ "List" ] "map"
                                , emptyRanged <| RecordAccessFunction ".name"
                                , emptyRanged <| FunctionOrValue "people"
                                ]
                            )
                        )
        , test "record access direct" <|
            \() ->
                parseFullStringWithNullState "(.spaceEvenly Internal.Style.classes)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (ParenthesizedExpression
                                ( emptyRange
                                , Application
                                    [ ( emptyRange, RecordAccessFunction ".spaceEvenly" )
                                    , ( emptyRange, QualifiedExpr [ "Internal", "Style" ] "classes" )
                                    ]
                                )
                            )
                        )
        , test "prefix notation" <|
            \() ->
                parseFullStringWithNullState "(::) x" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just <|
                            Application
                                [ emptyRanged <| PrefixOperator "::"
                                , emptyRanged <| FunctionOrValue "x"
                                ]
                        )
        , test "negated expression for value" <|
            \() ->
                parseFullStringWithNullState "-x" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just (Negation (emptyRanged <| FunctionOrValue "x")))
        , test "negated expression in application" <|
            \() ->
                parseFullStringWithNullState "toFloat -5" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Application
                                [ emptyRanged <| FunctionOrValue "toFloat"
                                , emptyRanged <| Negation (emptyRanged <| Integer 5)
                                ]
                            )
                        )
        , test "negated expression for parenthesized" <|
            \() ->
                parseFullStringWithNullState "-(x - y)" expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (Negation
                                ( emptyRange
                                , ParenthesizedExpression
                                    ( emptyRange
                                    , Application
                                        [ ( emptyRange, FunctionOrValue "x" )
                                        , ( emptyRange, Operator "-" )
                                        , ( emptyRange, FunctionOrValue "y" )
                                        ]
                                    )
                                )
                            )
                        )
        ]
