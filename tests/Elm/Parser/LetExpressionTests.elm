module Elm.Parser.LetExpressionTests exposing (all)

import Combine exposing (string)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (empty)
import Expect
import Test exposing (..)


all : Test
all =
    describe "LetExpressionTests"
        [ test "let body" <|
            \() ->
                """let
  foo = bar

  john = doe in 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                        (LetFunction
                                            { declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                    { arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } (FunctionOrValue [] "bar")
                                                    , name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    , Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                        (LetFunction
                                            { declaration =
                                                Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                    { arguments = []
                                                    , expression = Node { start = { row = 4, column = 10 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "doe")
                                                    , name = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } } (Integer 1)
                                }
                            )
                        )
        , test "correct let with indent" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n  bar" Parser.expression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                                    { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } <| FunctionOrValue [] "bar"
                                }
                            )
                        )
        , test "let with deindented expression in in" <|
            \() ->
                """let
  bar = 1
 in
   bar"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 4, column = 7 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                        (LetFunction
                                            { declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    { arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Integer 1)
                                                    , name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } } (FunctionOrValue [] "bar")
                                }
                            )
                        )
        , test "some let" <|
            \() ->
                parseFullStringState emptyState "let\n    _ = b\n in\n    z" Parser.expression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } } <|
                                        LetDestructuring
                                            (Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } AllPattern)
                                            (Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } <| FunctionOrValue [] "b")
                                    ]
                                , expression = Node { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } } <| FunctionOrValue [] "z"
                                }
                            )
                        )
        , test "let inlined" <|
            \() ->
                """let indent = String.length s in indent"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                        (LetFunction
                                            { declaration =
                                                Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                    { arguments = []
                                                    , expression =
                                                        Node { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                            (Application
                                                                [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } } (FunctionOrValue [ "String" ] "length")
                                                                , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (FunctionOrValue [] "s")
                                                                ]
                                                            )
                                                    , name = Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } } "indent"
                                                    }
                                            , documentation = Nothing
                                            , signature = Nothing
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } } (FunctionOrValue [] "indent")
                                }
                            )
                        )
        , test "let without indentation" <|
            \() ->
                parseFullStringState emptyState " let\n b = 1\n in\n b" (Layout.layout |> Combine.continueWith Parser.letExpression)
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node empty <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                Node empty <|
                                                    { name = Node empty "b"
                                                    , arguments = []
                                                    , expression = Node empty <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node empty <| FunctionOrValue [] "b"
                                }
                            )
                        )
        , test "let with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState " let\n b = 1\n in\n b\n\n\n\n--some comment\n" (Layout.layout |> Combine.continueWith Parser.letExpression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { start = { row = 1, column = 2 }, end = { row = 4, column = 3 } })
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parseFullStringWithNullState source Parser.expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected
