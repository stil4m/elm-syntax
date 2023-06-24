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
                                { args = [ Node empty UnitPattern ]
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
                                { args = [ Node empty (RecordPattern [ Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } "foo" ]) ]
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
        , test "function arg" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.functionArgument
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just (VarPattern "a"))
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
                parseFullStringState emptyState " \\a b -> a + b\n\n\n\n--some comment\n" (Layout.layout |> Combine.continueWith Parser.expression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { end = { column = 15, row = 1 }, start = { column = 2, row = 1 } })
        ]
