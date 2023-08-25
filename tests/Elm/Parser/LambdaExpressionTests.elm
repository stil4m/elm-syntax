module Elm.Parser.LambdaExpressionTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "LambdaExpressionTests"
        [ test "unit lambda" <|
            \() ->
                parseFullStringState emptyState "\\() -> foo" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args = [ Node.empty UnitPattern ]
                                , expression = Node empty <| FunctionOrValue [] "foo"
                                }
                            )
                        )
        , test "record lambda" <|
            \() ->
                parseFullStringState emptyState "\\{foo} -> foo" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args = [ Node empty (RecordPattern [ Node { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } "foo" ]) ]
                                , expression = Node empty <| FunctionOrValue [] "foo"
                                }
                            )
                        )
        , test "empty record lambda" <|
            \() ->
                parseFullStringState emptyState "\\{} -> foo" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args = [ Node empty (RecordPattern []) ]
                                , expression = Node empty <| FunctionOrValue [] "foo"
                                }
                            )
                        )
        , test "args lambda" <|
            \() ->
                parseFullStringState emptyState "\\a b -> a + b" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args =
                                    [ Node empty <| VarPattern "a"
                                    , Node empty <| VarPattern "b"
                                    ]
                                , expression =
                                    Node empty <|
                                        Application
                                            [ Node empty <| FunctionOrValue [] "a"
                                            , Node empty <| Operator "+"
                                            , Node empty <| FunctionOrValue [] "b"
                                            ]
                                }
                            )
                        )
        , test "tuple lambda" <|
            \() ->
                parseFullStringState emptyState "\\(a,b) -> a + b" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args =
                                    [ Node empty <|
                                        TuplePattern
                                            [ Node empty <| VarPattern "a"
                                            , Node empty <| VarPattern "b"
                                            ]
                                    ]
                                , expression = Node empty <| Application [ Node empty <| FunctionOrValue [] "a", Node empty <| Operator "+", Node empty <| FunctionOrValue [] "b" ]
                                }
                            )
                        )
        , test "lambda with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState """ \\a b -> a + b



--some comment
""" (Layout.layout |> Combine.continueWith Parser.expression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } })
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected
