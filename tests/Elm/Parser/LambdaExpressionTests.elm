module Elm.Parser.LambdaExpressionTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Expression as Parser
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
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
                                { firstArg = Node.empty UnitPattern_
                                , restOfArgs = []
                                , expression = Node.empty <| FunctionOrValue [] "foo"
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
                                { firstArg = Node.empty (RecordPattern_ [ Node { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } "foo" ])
                                , restOfArgs = []
                                , expression = Node.empty <| FunctionOrValue [] "foo"
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
                                { firstArg = Node.empty (RecordPattern_ [])
                                , restOfArgs = []
                                , expression = Node.empty <| FunctionOrValue [] "foo"
                                }
                            )
                        )
        , test "function arg" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.functionArgument
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just (VarPattern_ "a"))
        , test "args lambda" <|
            \() ->
                parseFullStringState emptyState "\\a b -> a + b" Parser.expression
                    |> Maybe.map (Node.value >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { firstArg =
                                    Node.empty <| VarPattern_ "a"
                                , restOfArgs = [ Node.empty <| VarPattern_ "b" ]
                                , expression =
                                    Node.empty <|
                                        Application
                                            (Node.empty <| FunctionOrValue [] "a")
                                            [ Node.empty <| Operator "+"
                                            , Node.empty <| FunctionOrValue [] "b"
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
                                { firstArg =
                                    Node.empty <|
                                        TuplePattern_
                                            [ Node.empty <| VarPattern_ "a"
                                            , Node.empty <| VarPattern_ "b"
                                            ]
                                , restOfArgs = []
                                , expression =
                                    Node.empty <|
                                        Application
                                            (Node.empty <| FunctionOrValue [] "a")
                                            [ Node.empty <| Operator "+"
                                            , Node.empty <| FunctionOrValue [] "b"
                                            ]
                                }
                            )
                        )
        , test "lambda with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState " \\a b -> a + b\n\n\n\n--some comment\n" (Layout.layout |> Combine.continueWith Parser.expression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { start = { row = 1, column = 2 }, end = { row = 1, column = 15 } })
        ]
