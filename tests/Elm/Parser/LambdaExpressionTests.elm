module Elm.Parser.LambdaExpressionTests exposing (all)

import Elm.Parser.Expression exposing (expression)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "LambdaExpressionTests"
        [ test "unit lambda" <|
            \() ->
                "\\() -> foo"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (LambdaExpression
                                { args = [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } UnitPattern ]
                                , expression = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "foo")
                                }
                            )
                        )
        , test "record lambda" <|
            \() ->
                "\\{foo} -> foo"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (LambdaExpression
                                { args = [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } (RecordPattern [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo" ]) ]
                                , expression = Node { start = { row = 1, column = 11 }, end = { row = 1, column = 14 } } (FunctionOrValue [] "foo")
                                }
                            )
                        )
        , test "empty record lambda" <|
            \() ->
                "\\{} -> foo"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                            (LambdaExpression
                                { args = [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } (RecordPattern []) ]
                                , expression = Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "foo")
                                }
                            )
                        )
        , test "args lambda" <|
            \() ->
                "\\a b -> a + b"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (LambdaExpression
                                { args =
                                    [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (VarPattern "a")
                                    , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (VarPattern "b")
                                    ]
                                , expression =
                                    Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                        (OperatorApplication "+"
                                            Left
                                            (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "a"))
                                            (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (FunctionOrValue [] "b"))
                                        )
                                }
                            )
                        )
        , test "tuple lambda" <|
            \() ->
                "\\(a,b) -> a + b"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                            (LambdaExpression
                                { args =
                                    [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } }
                                        (TuplePattern
                                            [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (VarPattern "a")
                                            , Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "b")
                                            ]
                                        )
                                    ]
                                , expression =
                                    Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } }
                                        (OperatorApplication "+"
                                            Left
                                            (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (FunctionOrValue [] "a"))
                                            (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 16 } } (FunctionOrValue [] "b"))
                                        )
                                }
                            )
                        )
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst expression
