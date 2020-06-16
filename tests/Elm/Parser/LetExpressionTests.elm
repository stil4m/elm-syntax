module Elm.Parser.LetExpressionTests exposing (all)

import Elm.Parser.Expression exposing (expression)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Expect
import Test exposing (..)


all : Test
all =
    describe "LetExpressionTests"
        [ test "let expression with multiple declarations" <|
            \() ->
                """let
  foo = bar

  john n = n in 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 4, column = 18 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 12 } }
                                                    { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "foo"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } } (FunctionOrValue [] "bar")
                                                    }
                                            }
                                        )
                                    , Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 4, column = 3 }, end = { row = 4, column = 13 } }
                                                    { name = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 7 } } "john"
                                                    , arguments = [ Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (VarPattern "n") ]
                                                    , expression = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "n")
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 4, column = 17 }, end = { row = 4, column = 18 } } (Integer 1)
                                }
                            )
                        )
        , test "Let with `in` indented more than the body and let declarations" <|
            \() ->
                """let
  bar = 1
            in
  bar"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 4, column = 6 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } } (FunctionOrValue [] "bar")
                                }
                            )
                        )
        , test "should fail to parse if declaration is indented as much as `let`" <|
            \() ->
                """  let
  bar = 1
  in
  bar"""
                    |> expectInvalid
        , test "should fail to parse if declarations are not indented the same way" <|
            \() ->
                """  let
    bar = 1
      foo = 2
  in
  bar"""
                    |> expectInvalid
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
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } }
                                                    { name = Node { start = { row = 2, column = 3 }, end = { row = 2, column = 6 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 4, column = 4 }, end = { row = 4, column = 7 } } (FunctionOrValue [] "bar")
                                }
                            )
                        )
        , test "Let function with type annotation" <|
            \() ->
                """let
    bar : Int
    bar = 1
  in
  bar"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 5, column = 6 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 5 }, end = { row = 3, column = 12 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                        { name = Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                        , typeAnnotation =
                                                            Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } }
                                                                (Type (Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                    { name = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 6 } } (FunctionOrValue [] "bar")
                                }
                            )
                        )
        , test "Let function with type annotation (separated by a few lines)" <|
            \() ->
                """let
    bar : Int


    bar = 1
  in
  bar"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 5 }, end = { row = 5, column = 12 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature =
                                                Just
                                                    (Node { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                                                        { name = Node { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } } "bar"
                                                        , typeAnnotation = Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } (Type (Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Node { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                    { name = Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } "bar"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 7, column = 3 }, end = { row = 7, column = 6 } } (FunctionOrValue [] "bar")
                                }
                            )
                        )
        , test "should fail to parse when type annotation and declaration are not aligned (annotation earlier)" <|
            \() ->
                """let
    bar : Int
      bar = 1
  in
  bar"""
                    |> expectInvalid
        , test "should fail to parse when type annotation and declaration are not aligned (annotation later)" <|
            \() ->
                """let
       bar : Int
    bar = 1
  in
  bar"""
                    |> expectInvalid

        -- TODO Make this pass
        --      , test "should fail to parse `as` pattern not surrounded by parentheses" <|
        --          \() ->
        --              """let
        --  bar n as m = 1
        --in
        --bar"""
        --                  |> expectInvalid
        , test "should not parse let destructuring with a type annotation" <|
            \() ->
                """let
    bar : Int
    (bar) = 1
  in
  bar"""
                    |> expectInvalid
        , test "should not parse let type annotation without a declaration" <|
            \() ->
                """let
    bar : Int
  in
  bar"""
                    |> expectInvalid
        , test "Using destructuring" <|
            \() ->
                """let
    _ = b
    {a} = b
    (c, d) = e
    (Node _ f) = g
 in
    1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 6 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
                                        (LetDestructuring (Node { start = { row = 2, column = 5 }, end = { row = 2, column = 6 } } AllPattern) (Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } (FunctionOrValue [] "b")))
                                    , Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                        (LetDestructuring
                                            (Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                                                (RecordPattern [ Node { start = { row = 3, column = 6 }, end = { row = 3, column = 7 } } "a" ])
                                            )
                                            (Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (FunctionOrValue [] "b"))
                                        )
                                    , Node { start = { row = 4, column = 5 }, end = { row = 4, column = 15 } }
                                        (LetDestructuring
                                            (Node { start = { row = 4, column = 5 }, end = { row = 4, column = 11 } }
                                                (TuplePattern
                                                    [ Node { start = { row = 4, column = 6 }, end = { row = 4, column = 7 } } (VarPattern "c")
                                                    , Node { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } } (VarPattern "d")
                                                    ]
                                                )
                                            )
                                            (Node { start = { row = 4, column = 14 }, end = { row = 4, column = 15 } } (FunctionOrValue [] "e"))
                                        )
                                    , Node { start = { row = 5, column = 5 }, end = { row = 5, column = 19 } }
                                        (LetDestructuring
                                            (Node { start = { row = 5, column = 5 }, end = { row = 5, column = 15 } }
                                                (ParenthesizedPattern
                                                    (Node { start = { row = 5, column = 6 }, end = { row = 5, column = 14 } } (NamedPattern { moduleName = [], name = "Node" } [ Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } AllPattern, Node { start = { row = 5, column = 13 }, end = { row = 5, column = 14 } } (VarPattern "f") ]))
                                                )
                                            )
                                            (Node { start = { row = 5, column = 18 }, end = { row = 5, column = 19 } } (FunctionOrValue [] "g"))
                                        )
                                    ]
                                , expression = Node { start = { row = 7, column = 5 }, end = { row = 7, column = 6 } } (Integer 1)
                                }
                            )
                        )
        , test "On one line" <|
            \() ->
                "let indent = String.length s in indent"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 1, column = 5 }, end = { row = 1, column = 29 } }
                                                    { name = Node { start = { row = 1, column = 5 }, end = { row = 1, column = 11 } } "indent"
                                                    , arguments = []
                                                    , expression =
                                                        Node { start = { row = 1, column = 14 }, end = { row = 1, column = 29 } }
                                                            (Application
                                                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 27 } } (FunctionOrValue [ "String" ] "length"))
                                                                [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (FunctionOrValue [] "s")
                                                                ]
                                                            )
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 1, column = 33 }, end = { row = 1, column = 39 } } (FunctionOrValue [] "indent")
                                }
                            )
                        )
        , test "let with list after in without space" <|
            \() ->
                """let
        a = 1
    in[]"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (ListExpr [])
                                }
                            )
                        )
        , test "let with record after in without space" <|
            \() ->
                """let
        a = 1
    in{}"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 9 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 9 } } (RecordExpr [])
                                }
                            )
                        )
        , test "let with lambda after in without space" <|
            \() ->
                """let
        a = 1
    in\\_ -> 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 14 } }
                            (LetExpression
                                { declarations =
                                    [ Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                        (LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                Node { start = { row = 2, column = 9 }, end = { row = 2, column = 14 } }
                                                    { name = Node { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } } "a"
                                                    , arguments = []
                                                    , expression = Node { start = { row = 2, column = 13 }, end = { row = 2, column = 14 } } (Integer 1)
                                                    }
                                            }
                                        )
                                    ]
                                , expression =
                                    Node { start = { row = 3, column = 7 }, end = { row = 3, column = 14 } }
                                        (LambdaExpression { args = [ Node { start = { row = 3, column = 8 }, end = { row = 3, column = 9 } } AllPattern ], expression = Node { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } } (Integer 1) })
                                }
                            )
                        )
        , test "let is not confused by a variable name starting with let" <|
            \() ->
                "letterbox"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "letterbox"))
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst =
    ParserWithCommentsUtil.expectAst expression


expectInvalid : String -> Expect.Expectation
expectInvalid =
    ParserWithCommentsUtil.expectInvalid expression
