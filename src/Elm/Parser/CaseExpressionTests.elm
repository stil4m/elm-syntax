module Elm.Parser.CaseExpressionTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "Case expression tests"
        [ test "case block" <|
            \() ->
                parseFullStringState emptyState "case True of" Parser.caseBlock
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal (Just (FunctionOrValue "True"))
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
                            ( ( emptyRange, NamedPattern (QualifiedNameRef [] "True") [] )
                            , emptyRanged <| Integer 1
                            )
                        )
        , test "caseStatement qualified" <|
            \() ->
                parseFullStringState emptyState "Foo.Bar -> 1" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( ( emptyRange, NamedPattern (QualifiedNameRef [ "Foo" ] "Bar") [] )
                            , emptyRanged <| Integer 1
                            )
                        )
        , test "caseStatement no spacing" <|
            \() ->
                parseFullStringState emptyState "32->Backspace" Parser.caseStatement
                    |> Maybe.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern)
                    |> Expect.equal
                        (Just
                            ( ( emptyRange, IntPattern 32 )
                            , emptyRanged <| FunctionOrValue "Backspace"
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
                            ( ( emptyRange, NamedPattern (QualifiedNameRef [] "True") [] )
                            , emptyRanged <| Integer 1
                            )
                        )
        , test "caseStatements" <|
            \() ->
                parseFullStringState emptyState "True -> 1\nFalse -> 2" Parser.caseStatements
                    |> Maybe.map (List.map (Tuple.mapSecond noRangeExpression >> Tuple.mapFirst noRangePattern))
                    |> Expect.equal
                        (Just
                            [ ( ( emptyRange, NamedPattern (QualifiedNameRef [] "True") [] )
                              , emptyRanged <| Integer 1
                              )
                            , ( ( emptyRange, NamedPattern (QualifiedNameRef [] "False") [] )
                              , emptyRanged <| Integer 2
                              )
                            ]
                        )
        , test "case expression" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n  False -> 2" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression = emptyRanged <| FunctionOrValue "f"
                                , cases =
                                    [ ( ( emptyRange, NamedPattern (QualifiedNameRef [] "True") [] )
                                      , emptyRanged <| Integer 1
                                      )
                                    , ( ( emptyRange, NamedPattern (QualifiedNameRef [] "False") [] )
                                      , emptyRanged <| Integer 2
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression (range)" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n  False -> 2" Parser.expression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression =
                                    ( { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } }
                                    , FunctionOrValue "f"
                                    )
                                , cases =
                                    [ ( ( { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } }
                                        , NamedPattern (QualifiedNameRef [] "True") []
                                        )
                                      , ( { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                                        , Integer 1
                                        )
                                      )
                                    , ( ( { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                        , NamedPattern (QualifiedNameRef [] "False") []
                                        )
                                      , ( { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                                        , Integer 2
                                        )
                                      )
                                    ]
                                }
                            )
                        )
        , test "case expression wrong - indent second case" <|
            \() ->
                parseFullStringState emptyState "case f of\n  True -> 1\n False -> 2" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal Nothing
        , test "update case expression" <|
            \() ->
                parseFullStringState emptyState "case msg of\n  Increment ->\n    model + 1\n  Decrement ->\n    model - 1" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (CaseExpression
                                { expression = emptyRanged <| FunctionOrValue "msg"
                                , cases =
                                    [ ( ( emptyRange, NamedPattern (QualifiedNameRef [] "Increment") [] )
                                      , emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "model"
                                                , emptyRanged <| Operator "+"
                                                , emptyRanged <| Integer 1
                                                ]
                                      )
                                    , ( ( emptyRange, NamedPattern (QualifiedNameRef [] "Decrement") [] )
                                      , emptyRanged <|
                                            Application
                                                [ emptyRanged <| FunctionOrValue "model"
                                                , emptyRanged <| Operator "-"
                                                , emptyRanged <| Integer 1
                                                ]
                                      )
                                    ]
                                }
                            )
                        )
        ]
