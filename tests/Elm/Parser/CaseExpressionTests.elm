module Elm.Parser.CaseExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
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
                parseFullStringWithNullState """case True of""" Parser.caseBlock
                    |> Maybe.map Node.value
                    |> Expect.equal (Just (FunctionOrValue [] "True"))
        , test "case block with wrong indent" <|
            \() ->
                parseFullStringWithNullState """case
True
of""" Parser.caseBlock
                    |> Expect.equal Nothing
        , test "caseStatement" <|
            \() ->
                parseFullStringWithNullState """True -> 1""" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } <| Integer 1
                            )
                        )
        , test "caseStatement qualified" <|
            \() ->
                parseFullStringWithNullState """Foo.Bar -> 1""" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } <| NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") []
                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } <| Integer 1
                            )
                        )
        , test "caseStatement no spacing" <|
            \() ->
                parseFullStringWithNullState """32->Backspace""" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } } <| IntPattern 32
                            , Node { start = { row = 1, column = 5 }, end = { row = 1, column = 14 } } <| FunctionOrValue [] "Backspace"
                            )
                        )
        , test "caseStatement wrong indent" <|
            \() ->
                parseFullStringWithNullState """True -> 
1""" Parser.caseStatement
                    |> Expect.equal Nothing
        , test "caseStatement correct on new line" <|
            \() ->
                parseFullStringWithNullState """True ->
  1""" Parser.caseStatement
                    |> Expect.equal
                        (Just
                            ( Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } <| NamedPattern (QualifiedNameRef [] "True") []
                            , Node { start = { row = 2, column = 3 }, end = { row = 2, column = 4 } } <| Integer 1
                            )
                        )
        , test "caseStatements" <|
            \() ->
                parseFullStringWithNullState """True -> 1
False -> 2""" Parser.caseStatements
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
                parseFullStringWithNullState """case f of
  True -> 1
  False -> 2""" Parser.expression
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
                parseFullStringWithNullState """case f of
  True -> 1
  False -> 2""" Parser.expression
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
                parseFullStringWithNullState """case f of
  True -> 1
  False -> 2

""" Parser.expression
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
                parseFullStringWithNullState """case f of
  True -> 1
 False -> 2""" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal Nothing
        , test "update case expression" <|
            \() ->
                parseFullStringWithNullState """case msg of
  Increment ->
    model + 1
  Decrement ->
    model - 1""" Parser.expression
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


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
