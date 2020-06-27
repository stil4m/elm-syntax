module Elm.Parser.LambdaExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.DestructurPattern exposing (DestructurPattern(..))
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
                                { args = [ Node emptyRange UnitPattern_ ]
                                , expression = Node emptyRange <| FunctionOrValue [] "foo"
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
                                { args =
                                    [ Node emptyRange <| VarPattern_ "a"
                                    , Node emptyRange <| VarPattern_ "b"
                                    ]
                                , expression =
                                    Node emptyRange <|
                                        Application
                                            (Node emptyRange <| FunctionOrValue [] "a")
                                            [ Node emptyRange <| Operator "+"
                                            , Node emptyRange <| FunctionOrValue [] "b"
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
                                    [ Node emptyRange <|
                                        TuplePattern_
                                            [ Node emptyRange <| VarPattern_ "a"
                                            , Node emptyRange <| VarPattern_ "b"
                                            ]
                                    ]
                                , expression = Node emptyRange <| Application (Node emptyRange <| FunctionOrValue [] "a") [ Node emptyRange <| Operator "+", Node emptyRange <| FunctionOrValue [] "b" ]
                                }
                            )
                        )
        ]
