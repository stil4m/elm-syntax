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
import Elm.Syntax.Range exposing (empty)
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
                            [ Node empty <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node empty <|
                                            { name = Node empty "foo"
                                            , arguments = []
                                            , expression = Node empty <| FunctionOrValue [] "bar"
                                            }
                                    }
                            , Node empty <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node empty <|
                                            { name = Node empty "john"
                                            , arguments = []
                                            , expression = Node empty <| FunctionOrValue [] "doe"
                                            }
                                    }
                            ]
                        )
        , test "let block" <|
            \() ->
                parseFullStringState emptyState "let\n  foo = bar\n  \n  john = doe\n in" Parser.letBlock
                    |> Expect.equal
                        (Just
                            [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } } <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                            { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                            , arguments = []
                                            , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } <| FunctionOrValue [] "bar"
                                            }
                                    }
                            , Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } } <|
                                LetFunction
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } } <|
                                            { name = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                            , arguments = []
                                            , expression = Node { start = { row = 4, column = 10 }, end = { row = 4, column = 13 } } <| FunctionOrValue [] "doe"
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
                            (Let
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } <|
                                                    { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } <| IntegerLiteral 1
                                                    }
                                            }
                                    ]
                                , expression = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } <| FunctionOrValue [] "bar"
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
                            (Let
                                { declarations =
                                    [ Node empty <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node empty <|
                                                    { name = Node empty "bar"
                                                    , arguments = []
                                                    , expression = Node empty <| IntegerLiteral 1
                                                    }
                                            }
                                    ]
                                , expression = Node empty <| FunctionOrValue [] "bar"
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
                            (ListLiteral
                                [ Node empty <|
                                    Let
                                        { declarations =
                                            [ Node empty <|
                                                LetFunction
                                                    { documentation = Nothing
                                                    , signature = Nothing
                                                    , declaration =
                                                        Node empty
                                                            { name = Node empty "bar"
                                                            , arguments = []
                                                            , expression = Node empty <| IntegerLiteral 1
                                                            }
                                                    }
                                            ]
                                        , expression = Node empty <| FunctionOrValue [] "bar"
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
                            (Let
                                { declarations =
                                    [ Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } } <|
                                        LetDestructuring
                                            (Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } AllPattern_)
                                            (Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } <| FunctionOrValue [] "b")
                                    ]
                                , expression = Node { start = { row = 4, column = 5 }, end = { row = 4, column = 6 } } <| FunctionOrValue [] "z"
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
                            (Let
                                { declarations =
                                    [ Node empty <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node empty <|
                                                    { name = Node empty "indent"
                                                    , arguments = []
                                                    , expression =
                                                        Node empty <|
                                                            Application
                                                                (Node empty <| FunctionOrValue [ "String" ] "length")
                                                                [ Node empty <| FunctionOrValue [] "s" ]
                                                    }
                                            }
                                    ]
                                , expression = Node empty <| FunctionOrValue [] "indent"
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
                            (Let
                                { declarations =
                                    [ Node empty <|
                                        LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Nothing
                                            , declaration =
                                                Node empty <|
                                                    { name = Node empty "indent"
                                                    , arguments = []
                                                    , expression = Node empty <| IntegerLiteral 1
                                                    }
                                            }
                                    ]
                                , expression = Node empty <| FunctionOrValue [] "indent"
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
                            (Let
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
                                                    , expression = Node empty <| IntegerLiteral 1
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
