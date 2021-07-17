module Elm.Parser.LetExpressionTests exposing (all)

import Combine exposing (string)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Expect
import Test exposing (..)


all : Test
all =
    describe "LetExpressionTests"
        [ test "let body" <|
            \() ->
                parseFullStringState emptyState "foo = bar\n  \n  john = doe" (pushIndent 2 Parser.letBody)
                    |> Maybe.map (List.map noRangeLetDeclaration)
                    |> Expect.equal
                        (Just
                            [ Node emptyRange <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node emptyRange <|
                                            { name = Node emptyRange "foo"
                                            , arguments = []
                                            , expression = Node emptyRange <| FunctionOrValue [] "bar"
                                            }
                                    }
                            , Node emptyRange <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node emptyRange <|
                                            { name = Node emptyRange "john"
                                            , arguments = []
                                            , expression = Node emptyRange <| FunctionOrValue [] "doe"
                                            }
                                    }
                            ]
                        )
        , test "let block" <|
            \() ->
                parseFullStringState emptyState "let\n  foo = bar\n  \n  john = doe\n in" Parser.letBlock
                    |> Expect.equal
                        (Just
                            [ Node { end = { column = 12, row = 2 }, start = { column = 3, row = 2 } } <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { end = { column = 12, row = 2 }, start = { column = 3, row = 2 } }
                                            { name = Node { end = { column = 6, row = 2 }, start = { column = 3, row = 2 } } "foo"
                                            , arguments = []
                                            , expression = Node { end = { column = 12, row = 2 }, start = { column = 9, row = 2 } } <| FunctionOrValue [] "bar"
                                            }
                                    }
                            , Node { end = { column = 13, row = 4 }, start = { column = 3, row = 4 } } <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { end = { column = 13, row = 4 }, start = { column = 3, row = 4 } } <|
                                            { name = Node { end = { column = 7, row = 4 }, start = { column = 3, row = 4 } } "john"
                                            , arguments = []
                                            , expression = Node { end = { column = 13, row = 4 }, start = { column = 10, row = 4 } } <| FunctionOrValue [] "doe"
                                            }
                                    }
                            ]
                        )
        , test "correct let with indent" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n  bar" Parser.expression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node { end = { column = 10, row = 2 }, start = { column = 3, row = 2 } } <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { end = { column = 10, row = 2 }, start = { column = 3, row = 2 } } <|
                                                    { name = Node { end = { column = 6, row = 2 }, start = { column = 3, row = 2 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { end = { column = 10, row = 2 }, start = { column = 9, row = 2 } } <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node { end = { column = 6, row = 4 }, start = { column = 3, row = 4 } } <| FunctionOrValue [] "bar"
                                }
                            )
                        )
        , test "let with deindented expression in in" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n   bar" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node emptyRange <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node emptyRange <|
                                                    { name = Node emptyRange "bar"
                                                    , arguments = []
                                                    , expression = Node emptyRange <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node emptyRange <| FunctionOrValue [] "bar"
                                }
                            )
                        )
        , test "let in list" <|
            \() ->
                parseFullStringState emptyState "[\n  let\n    bar = 1\n  in\n    bar\n ]" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (ListExpr
                                [ Node emptyRange <|
                                    LetExpression
                                        { declarations =
                                            [ Node emptyRange <|
                                                LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Node emptyRange
                                                            { name = Node emptyRange "bar"
                                                            , arguments = []
                                                            , expression = Node emptyRange <| Integer 1
                                                            }
                                                    }
                                            ]
                                        , expression = Node emptyRange <| FunctionOrValue [] "bar"
                                        }
                                ]
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
                                    [ Node { end = { column = 10, row = 2 }, start = { column = 5, row = 2 } } <|
                                        LetDestructuring
                                            (Node { end = { column = 6, row = 2 }, start = { column = 5, row = 2 } } AllPattern_)
                                            (Node { end = { column = 10, row = 2 }, start = { column = 9, row = 2 } } <| FunctionOrValue [] "b")
                                    ]
                                , expression = Node { end = { column = 6, row = 4 }, start = { column = 5, row = 4 } } <| FunctionOrValue [] "z"
                                }
                            )
                        )
        , test "let inlined" <|
            \() ->
                parseFullStringState emptyState "let indent = String.length s in indent" Parser.expression
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node emptyRange <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node emptyRange <|
                                                    { name = Node emptyRange "indent"
                                                    , arguments = []
                                                    , expression =
                                                        Node emptyRange <|
                                                            Application
                                                                (Node emptyRange <| FunctionOrValue [ "String" ] "length")
                                                                [ Node emptyRange <| FunctionOrValue [] "s" ]
                                                    }
                                            }
                                    ]
                                , expression = Node emptyRange <| FunctionOrValue [] "indent"
                                }
                            )
                        )
        , test "let starting after definition" <|
            \() ->
                parseFullStringState emptyState "foo = let\n  indent = 1\n in\n indent" (functionName |> Combine.continueWith (string " = ") |> Combine.continueWith Parser.expression)
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node emptyRange <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                Node emptyRange <|
                                                    { name = Node emptyRange "indent"
                                                    , arguments = []
                                                    , expression = Node emptyRange <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node emptyRange <| FunctionOrValue [] "indent"
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
                                    [ Node emptyRange <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                Node emptyRange <|
                                                    { name = Node emptyRange "b"
                                                    , arguments = []
                                                    , expression = Node emptyRange <| Integer 1
                                                    }
                                            }
                                    ]
                                , expression = Node emptyRange <| FunctionOrValue [] "b"
                                }
                            )
                        )
        , test "let with trailing whitespace" <|
            \() ->
                parseFullStringState emptyState " let\n b = 1\n in\n b\n\n\n\n--some comment\n" (Layout.layout |> Combine.continueWith Parser.letExpression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { end = { column = 3, row = 4 }, start = { column = 2, row = 1 } })
        ]
