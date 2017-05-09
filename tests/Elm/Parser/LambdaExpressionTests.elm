module Elm.Parser.LambdaExpressionTests exposing (..)

import Elm.Parser.CombineTestUtil exposing (..)
import Expect
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Syntax.Range exposing (..)
import Test exposing (..)
import Elm.Parser.State exposing (emptyState)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)


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
                                { args = [ UnitPattern emptyRange ]
                                , expression = emptyRanged <| FunctionOrValue "foo"
                                }
                            )
                        )
        , test "args lambda" <|
            \() ->
                parseFullStringState emptyState "\\a b -> a + b" Parser.expression
                    |> Maybe.map (Tuple.second >> noRangeInnerExpression)
                    |> Expect.equal
                        (Just
                            (LambdaExpression
                                { args =
                                    [ VarPattern "a" emptyRange
                                    , VarPattern "b" emptyRange
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
                                    [ TuplePattern
                                        [ VarPattern "a" emptyRange
                                        , VarPattern "b" emptyRange
                                        ]
                                        emptyRange
                                    ]
                                , expression = emptyRanged <| Application [ emptyRanged <| FunctionOrValue "a", emptyRanged <| Operator "+", emptyRanged <| FunctionOrValue "b" ]
                                }
                            )
                        )
        ]
