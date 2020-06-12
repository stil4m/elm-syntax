module Elm.Parser.LetExpressionTests exposing (all)

import Combine exposing (string)
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser exposing (..)
import Elm.Parser.Layout as Layout
import Elm.Parser.State exposing (emptyState)
import Elm.Parser.Tokens exposing (functionName)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
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
        , test "correct let with indent" <|
            \() ->
                parseFullStringState emptyState "let\n  bar = 1\n in\n  bar" Parser.expression
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
                    |> Maybe.map noRangeExpression
                    |> Maybe.map Node.value
                    |> Expect.equal
                        (Just
                            (LetExpression
                                { declarations =
                                    [ Node emptyRange <|
                                        LetDestructuring
                                            (Node emptyRange AllPattern)
                                            (Node emptyRange <| FunctionOrValue [] "b")
                                    ]
                                , expression = Node emptyRange <| FunctionOrValue [] "z"
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
        ]
