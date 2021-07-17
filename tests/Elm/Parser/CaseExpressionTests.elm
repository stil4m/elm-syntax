module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Case expression tests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "case True of" Parser.caseBlock
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal (Just (FunctionOrValue [] "True"))
        , test "case block with wrong indent" <|
            \() ->
                parseFullStringState emptyState "case\nTrue\nof" Parser.caseBlock
                    |> Expect.equal Nothing
        , test "caseStatement" <|
            \() ->
                parseFullStringState emptyState "True -> 1" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node emptyRange <| Integer 1
                            )
                        )
        , test "caseStatement qualified" <|
            \() ->
                parseFullStringState emptyState "Foo.Bar -> 1" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( Node emptyRange <| NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") []
                            , Node emptyRange <| Integer 1
                            )
                        )
        , test "caseStatement no spacing" <|
            \() ->
                parseFullStringState emptyState "32->Backspace" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( Node emptyRange <| IntPattern 32
                            , Node emptyRange <| FunctionOrValue [] "Backspace"
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
                            ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node emptyRange <| Integer 1
                            )
                        )
        , test "caseStatements" <|
            \() ->
                parseFullStringState emptyState "True -> 1\nFalse -> 2" Parser.caseStatements
                    |> Maybe.map
                        (\( head, rest ) ->
                            ( Tuple.mapBoth noRangePattern noRangeExpression head
                            , List.map (Tuple.mapBoth noRangePattern noRangeExpression) rest
                            )
                        )
                    |> Expect.equal
                        (Just
                            ( ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "True") []
                              , Node emptyRange <| Integer 1
                              )
                            , [ ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "False") []
                                , Node emptyRange <| Integer 2
                                )
                              ]
                            )
                        )
        , test "many caseStatements" <|
            \() ->
                parseFullStringState emptyState "True -> 1\nFalse -> 2\nFalse -> 3" Parser.caseStatements
                    |> Maybe.map
                        (\( head, rest ) ->
                            ( Tuple.mapBoth noRangePattern noRangeExpression head
                            , List.map (Tuple.mapBoth noRangePattern noRangeExpression) rest
                            )
                        )
                    |> Expect.equal
                        (Just
                            ( ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "True") []
                              , Node emptyRange <| Integer 1
                              )
                            , [ ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "False") []
                                , Node emptyRange <| Integer 2
                                )
                              , ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "False") []
                                , Node emptyRange <| Integer 3
                                )
                              ]
                            )
                        )
        , test "case expression" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n  False -> 2" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression = Node emptyRange <| FunctionOrValue [] "f"
                                , firstCase =
                                    ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "True") []
                                    , Node emptyRange <| Integer 1
                                    )
                                , restOfCases =
                                    [ ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "False") []
                                      , Node emptyRange <| Integer 2
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
                            (Node { end = { column = 13, row = 3 }, leadingWhitespace = "", start = { column = 1, row = 1 } }
                                (CaseExpression
                                    { expression =
                                        Node { leadingWhitespace = "", start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                            FunctionOrValue [] "f"
                                    , firstCase =
                                        ( Node { leadingWhitespace = "", start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                            NamedPattern (QualifiedNameRef [] "True") []
                                        , Node { leadingWhitespace = "", start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                            Integer 1
                                        )
                                    , restOfCases =
                                        [ ( Node { leadingWhitespace = "", start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                                NamedPattern (QualifiedNameRef [] "False") []
                                          , Node { leadingWhitespace = "", start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
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
                            (Node { end = { column = 13, row = 3 }, leadingWhitespace = "", start = { column = 1, row = 1 } }
                                (CaseExpression
                                    { expression =
                                        Node { leadingWhitespace = "", start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } <|
                                            FunctionOrValue [] "f"
                                    , firstCase =
                                        ( Node { leadingWhitespace = "", start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } <|
                                            NamedPattern (QualifiedNameRef [] "True") []
                                        , Node { leadingWhitespace = "", start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } <|
                                            Integer 1
                                        )
                                    , restOfCases =
                                        [ ( Node { leadingWhitespace = "", start = { row = 3, column = 3 }, end = { row = 3, column = 8 } } <|
                                                NamedPattern (QualifiedNameRef [] "False") []
                                          , Node { leadingWhitespace = "", start = { row = 3, column = 12 }, end = { row = 3, column = 13 } } <|
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
                                { expression = Node emptyRange <| FunctionOrValue [] "msg"
                                , firstCase =
                                    ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "Increment") []
                                    , Node emptyRange <|
                                        Application
                                            (Node emptyRange <| FunctionOrValue [] "model")
                                            [ Node emptyRange <| Operator "+"
                                            , Node emptyRange <| Integer 1
                                            ]
                                    )
                                , restOfCases =
                                    [ ( Node emptyRange <| NamedPattern (QualifiedNameRef [] "Decrement") []
                                      , Node emptyRange <|
                                            Application
                                                (Node emptyRange <| FunctionOrValue [] "model")
                                                [ Node emptyRange <| Operator "-"
                                                , Node emptyRange <| Integer 1
                                                ]
                                      )
                                    ]
                                }
                            )
                        )
        ]
