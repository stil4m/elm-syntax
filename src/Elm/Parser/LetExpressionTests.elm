module Elm.Parser.LetExpressionTests exposing (..)

import Combine exposing (string)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


main =
    Tuple.second all


all : Test
all =
    describe "LetExpressionTests"
        [ test "let body" <|
            \() ->
                parseFullStringState emptyState "foo = bar\n  \n  john = doe" (pushIndent 2 Parser.letBody)
                    |> Maybe.map (List.map noRangeLetDeclaration)
                    |> Expect.equal
                        (Just
                            [ ( emptyRange
                              , LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { name = { value = "foo", range = emptyRange }
                                        , arguments = []
                                        , expression = emptyRanged <| FunctionOrValue "bar"
                                        }
                                    }
                              )
                            , ( emptyRange
                              , LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { name = { value = "john", range = emptyRange }
                                        , arguments = []
                                        , expression = emptyRanged <| FunctionOrValue "doe"
                                        }
                                    }
                              )
                            ]
                        )
        , test "let block" <|
            \() ->
                parseFullStringState emptyState "let\n  foo = bar\n  \n  john = doe\n in" Parser.letBlock
                    |> Maybe.map (List.map noRangeLetDeclaration)
                    |> Expect.equal
                        (Just
                            [ ( emptyRange
                              , LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { name = { value = "foo", range = emptyRange }
                                        , arguments = []
                                        , expression = emptyRanged <| FunctionOrValue "bar"
                                        }
                                    }
                              )
                            , ( emptyRange
                              , LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        { name = { value = "john", range = emptyRange }
                                        , arguments = []
                                        , expression = emptyRanged <| FunctionOrValue "doe"
                                        }
                                    }
                              )
                            ]
                        )
        , test "correct let with indent" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n  bar" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { name =
                                                    { value = "bar"
                                                    , range = emptyRange
                                                    }
                                                , arguments = []
                                                , expression = emptyRanged <| Integer 1
                                                }
                                            }
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "bar"
                                }
                            )
                        )
        , test "let with deindented expression in in" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n   bar" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { name = { value = "bar", range = emptyRange }
                                                , arguments = []
                                                , expression = emptyRanged <| Integer 1
                                                }
                                            }
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "bar"
                                }
                            )
                        )
        , test "let in list" <|
            \() ->
                parseFullStringState emptyState "[\n  let\n    bar = 1\n  in\n    bar\n ]" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (ListExpr
                                [ emptyRanged <|
                                    LetExpression
                                        { declarations =
                                            [ ( emptyRange
                                              , LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        { name = { value = "bar", range = emptyRange }
                                                        , arguments = []
                                                        , expression = emptyRanged <| Integer 1
                                                        }
                                                    }
                                              )
                                            ]
                                        , expression = emptyRanged <| FunctionOrValue "bar"
                                        }
                                ]
                            )
                        )
        , test "some let" <|
            \() ->
                parseFullStringState emptyState "let\n    _ = b\n in\n    z" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetDestructuring
                                            ( emptyRange, AllPattern )
                                            (emptyRanged <| FunctionOrValue "b")
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "z"
                                }
                            )
                        )
        , test "let inlined" <|
            \() ->
                parseFullStringState emptyState "let indent = String.length s in indent" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                { name = { value = "indent", range = emptyRange }
                                                , arguments = []
                                                , expression =
                                                    emptyRanged <|
                                                        Application [ emptyRanged <| QualifiedExpr [ "String" ] "length", emptyRanged <| FunctionOrValue "s" ]
                                                }
                                            }
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "indent"
                                }
                            )
                        )
        , test "let starting after definition" <|
            \() ->
                parseFullStringState emptyState "foo = let\n  indent = 1\n in\n indent" (functionName |> Combine.continueWith (string " = ") |> Combine.continueWith Parser.expression)
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                { name =
                                                    { value = "indent"
                                                    , range = emptyRange
                                                    }
                                                , arguments = []
                                                , expression = emptyRanged <| Integer 1
                                                }
                                            }
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "indent"
                                }
                            )
                        )
        , test "let without indentation" <|
            \() ->
                parseFullStringState emptyState " let\n b = 1\n in\n b" (Layout.layout |> Combine.continueWith Parser.letExpression)
                    |> Maybe.map (\a -> ( emptyRange, a ) |> noRangeExpression)
                    |> Maybe.map Tuple.second
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ ( emptyRange
                                      , LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                { name =
                                                    { value = "b"
                                                    , range = emptyRange
                                                    }
                                                , arguments = []
                                                , expression = emptyRanged <| Integer 1
                                                }
                                            }
                                      )
                                    ]
                                , expression = emptyRanged <| FunctionOrValue "b"
                                }
                            )
                        )
        ]
