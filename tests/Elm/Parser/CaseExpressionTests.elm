module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Case expression tests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "case True of" Parser.caseBlock
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [] "True"))
        , test "case block with wrong indent" <|
            \() ->
                parseFullStringState emptyState "case\nTrue\nof" Parser.caseBlock
                    |> Expect.equal Nothing
        , test "caseStatement" <|
            \() ->
                parseFullStringState emptyState "True -> 1" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| Integer 1
                            )
                        )
        , test "caseStatement qualified" <|
            \() ->
                parseFullStringState emptyState "Foo.Bar -> 1" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } <| NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") []
                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } <| Integer 1
                            )
                        )
        , test "caseStatement no spacing" <|
            \() ->
                parseFullStringState emptyState "32->Backspace" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } <| IntPattern 32
                            , Node { start = { row = 1, column = 5 }, end = { row = 1, column = 14 } } <| FunctionOrValue [] "Backspace"
                            )
                        )
        , test "caseStatement wrong indent" <|
            \() ->
                parseFullStringState emptyState "True -> \n1" Parser.caseStatement
                    |> Expect.equal Nothing
        , test "caseStatement correct on new line" <|
            \() ->
                parseFullStringState emptyState "True ->\n  1" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( Node.empty <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node.empty <| Integer 1
                            )
                        )
        , test "caseStatements" <|
            \() ->
                parseFullStringState emptyState "True -> 1\nFalse -> 2" Parser.caseStatements
                    |> Maybe.map (List.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern))
                    |> Expect.equal
                        (Just
                            [ ( Node.empty <| NamedPattern (QualifiedNameRef [] "True") []
                              , Node.empty <| Integer 1
                              )
                            , ( Node.empty <| NamedPattern (QualifiedNameRef [] "False") []
                              , Node.empty <| Integer 2
                              )
                            ]
                        )
        , test "case expression" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n  False -> 2" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression = Node.empty <| FunctionOrValue [] "f"
                                , cases =
                                    [ ( Node.empty <| NamedPattern (QualifiedNameRef [] "True") []
                                      , Node.empty <| Integer 1
                                      )
                                    , ( Node empty <| NamedPattern (QualifiedNameRef [] "False") []
                                      , Node empty <| Integer 2
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression (range)" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n  False -> 2" Parser.expression
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                (CaseExpression
                                    { expression =
                                        Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                            FunctionOrValue [] "f"
                                    , cases =
                                        [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                                NamedPattern (QualifiedNameRef [] "True") []
                                          , Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                                Integer 1
                                          )
                                        , ( Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                                NamedPattern (QualifiedNameRef [] "False") []
                                          , Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
                                                Integer 2
                                          )
                                        ]
                                    }
                                )
                            )
                        )
        , test "case expression (range) - with trailing whitespace" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "case f of\n  True -> 1\n  False -> 2\n\n" Parser.expression
                    |> Expect.equal
                        (Just
                            (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 13 } }
                                (CaseExpression
                                    { expression =
                                        Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                            FunctionOrValue [] "f"
                                    , cases =
                                        [ ( Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                                NamedPattern (QualifiedNameRef [] "True") []
                                          , Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                                Integer 1
                                          )
                                        , ( Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                                NamedPattern (QualifiedNameRef [] "False") []
                                          , Node { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
                                                Integer 2
                                          )
                                        ]
                                    }
                                )
                            )
                        )
        , test "case expression wrong - indent second case" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n False -> 2" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal Nothing
        , test "update case expression" <|
            \() ->
                parseFullStringState emptyState "case msg of\n  Increment ->\n    model + 1\n  Decrement ->\n    model - 1" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression = Node empty <| FunctionOrValue [] "msg"
                                , cases =
                                    [ ( Node empty <| NamedPattern (QualifiedNameRef [] "Increment") []
                                      , Node empty <|
                                            Application
                                                [ Node empty <| FunctionOrValue [] "model"
                                                , Node empty <| Operator "+"
                                                , Node empty <| Integer 1
                                                ]
                                      )
                                    , ( Node empty <| NamedPattern (QualifiedNameRef [] "Decrement") []
                                      , Node empty <|
                                            Application
                                                [ Node empty <| FunctionOrValue [] "model"
                                                , Node empty <| Operator "-"
                                                , Node empty <| Integer 1
                                                ]
                                      )
                                    ]
                                }
                            )
                        )
        ]
