module Elm.Parser.LambdaExpressionTests exposing (all)

import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Expression exposing (..)
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
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args = [ ( emptyRange, UnitPattern ) ]
                                , expression = emptyRanged <| FunctionOrValue "foo"
                                }
                            )
                        )
        , test "function arg" <|
            \() ->
                parseAsFarAsPossibleWithState emptyState "a b" Parser.functionArgument
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just (VarPattern "a"))
        , test "args lambda" <|
            \() ->
                parseFullStringState emptyState "\\a b -> a + b" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args =
                                    [ ( emptyRange, VarPattern "a" )
                                    , ( emptyRange, VarPattern "b" )
                                    ]
                                , expression =
                                    emptyRanged <|
                                        Application
                                            [ emptyRanged <| FunctionOrValue "a"
                                            , emptyRanged <| Operator "+"
                                            , emptyRanged <| FunctionOrValue "b"
                                            ]
                                }
                            )
                        )
        , test "tuple lambda" <|
            \() ->
                parseFullStringState emptyState "\\(a,b) -> a + b" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args =
                                    [ ( emptyRange
                                      , TuplePattern
                                            [ ( emptyRange, VarPattern "a" )
                                            , ( emptyRange, VarPattern "b" )
                                            ]
                                      )
                                    ]
                                , expression = emptyRanged <| Application [ emptyRanged <| FunctionOrValue "a", emptyRanged <| Operator "+", emptyRanged <| FunctionOrValue "b" ]
                                }
                            )
                        )
        ]
