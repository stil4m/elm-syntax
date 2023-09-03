module Elm.Parser.LetExpressionTests exposing (all)

import Combine
import Elm.Parser.CombineTestUtil exposing (..)
import Elm.Parser.Declarations as Parser
import Elm.Parser.Layout as Layout
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node as Node exposing (Node(..))
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
                                                    { arguments = [ Node { start = { row = 4, column = 8 }, end = { row = 4, column = 9 } } (VarPattern "n") ]
                                                    , expression = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "n")
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
                                                                (Typed (Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Node { start = { row = 3, column = 5 }, end = { row = 3, column = 12 } }
                                                    { arguments = []
                                                    , expression = Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Integer 1)
                                                    , name = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } "bar"
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
                                                        , typeAnnotation = Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } (Typed (Node { start = { row = 2, column = 11 }, end = { row = 2, column = 14 } } ( [], "Int" )) [])
                                                        }
                                                    )
                                            , declaration =
                                                Node { start = { row = 5, column = 5 }, end = { row = 5, column = 12 } }
                                                    { arguments = []
                                                    , expression = Node { start = { row = 5, column = 11 }, end = { row = 5, column = 12 } } (Integer 1)
                                                    , name = Node { start = { row = 5, column = 5 }, end = { row = 5, column = 8 } } "bar"
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
        , test "let with trailing whitespace" <|
            \() ->
                parse " let\n b = 1\n in\n b\n\n\n\n--some comment\n" (Layout.layout |> Combine.continueWith Parser.expression)
                    |> Maybe.map Node.range
                    |> Expect.equal (Just { start = { row = 1, column = 2 }, end = { row = 4, column = 3 } })
        ]


expectAst : Node Expression -> String -> Expect.Expectation
expectAst expected source =
    case parse source Parser.expression of
        Nothing ->
            Expect.fail "Expected the source to be parsed correctly"

        Just actual ->
            actual
                |> Expect.equal expected


expectInvalid : String -> Expect.Expectation
expectInvalid source =
    case parse source Parser.expression of
        Nothing ->
            Expect.pass

        Just actual ->
            Expect.fail ("This source code is successfully parsed but it shouldn't:\n" ++ Debug.toString actual)
