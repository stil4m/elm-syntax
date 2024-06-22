module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.CombineTestUtil as CombineTestUtil
import Elm.Parser.Declarations exposing (..)
import Elm.Syntax.Declaration exposing (..)
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.TypeAnnotation exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "DeclarationTests"
        [ test "function declaration" <|
            \() ->
                "foo = bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "bar")
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function declaration with empty record" <|
            \() ->
                "foo = {}"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                        { arguments = []
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (RecordExpr [])
                                        }
                                }
                            )
                        )
        , test "function with case in let" <|
            \() ->
                """inc x =
  let
    y =
      case x of
        True -> z
    a = b
  in a"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                            (FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 7 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 7 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                            (LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Node { start = { row = 3, column = 5 }, end = { row = 5, column = 18 } }
                                                                        { name = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 6 } } "y"
                                                                        , arguments = []
                                                                        , expression =
                                                                            Node { start = { row = 4, column = 7 }, end = { row = 5, column = 18 } }
                                                                                (CaseExpression
                                                                                    { expression = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "x")
                                                                                    , cases =
                                                                                        [ ( Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                                          , Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (FunctionOrValue [] "z")
                                                                                          )
                                                                                        ]
                                                                                    }
                                                                                )
                                                                        }
                                                                }
                                                            )
                                                        , Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                            (LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Node { start = { row = 6, column = 5 }, end = { row = 6, column = 10 } }
                                                                        { name = Node { start = { row = 6, column = 5 }, end = { row = 6, column = 6 } } "a"
                                                                        , arguments = []
                                                                        , expression = Node { start = { row = 6, column = 9 }, end = { row = 6, column = 10 } } (FunctionOrValue [] "b")
                                                                        }
                                                                }
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 7, column = 6 }, end = { row = 7, column = 7 } } (FunctionOrValue [] "a")
                                                    }
                                                )
                                        }
                                }
                            )
                        )
        , test "function declaration with args" <|
            \() ->
                "inc x = x + 1"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                            (FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 14 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "inc"
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern "x") ]
                                        , expression =
                                            Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (OperatorApplication "+"
                                                    Infix.Left
                                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "x"))
                                                    (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (Integer 1))
                                                )
                                        }
                                }
                            )
                        )
        , test "function declaration with let" <|
            \() ->
                """foo =
 let
  b = 1
 in
  b"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                            (LetFunction
                                                                { declaration =
                                                                    Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                        { arguments = []
                                                                        , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (Integer 1)
                                                                        , name = Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                        }
                                                                , documentation = Nothing
                                                                , signature = Nothing
                                                                }
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "let destructuring with no spaces around '='" <|
            \() ->
                """foo =
 let
  (b, c)=(1, 2)
 in
  b"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                            (LetDestructuring
                                                                (Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                    (TuplePattern
                                                                        [ Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (VarPattern "b")
                                                                        , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (VarPattern "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                    (TupledExpression
                                                                        [ Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (Integer 1)
                                                                        , Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (Integer 2)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "declaration with record" <|
            \() ->
                """main =
  beginnerProgram { model = 0, view = view, update = update }"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                (Application
                                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (FunctionOrValue [] "beginnerProgram")
                                                    , Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                        (RecordExpr
                                                            [ Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                ( Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                , Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (Integer 0)
                                                                )
                                                            , Node { start = { row = 2, column = 32 }, end = { row = 2, column = 43 } }
                                                                ( Node { start = { row = 2, column = 32 }, end = { row = 2, column = 36 } } "view"
                                                                , Node { start = { row = 2, column = 39 }, end = { row = 2, column = 43 } } (FunctionOrValue [] "view")
                                                                )
                                                            , Node { start = { row = 2, column = 45 }, end = { row = 2, column = 61 } }
                                                                ( Node { start = { row = 2, column = 45 }, end = { row = 2, column = 51 } } "update"
                                                                , Node { start = { row = 2, column = 54 }, end = { row = 2, column = 60 } } (FunctionOrValue [] "update")
                                                                )
                                                            ]
                                                        )
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "update function" <|
            \() ->
                """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (CaseExpression
                                                    { cases =
                                                        [ ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                                          , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                (OperatorApplication "+"
                                                                    Left
                                                                    (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Integer 1))
                                                                )
                                                          )
                                                        , ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (OperatorApplication "-"
                                                                    Left
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Integer 1))
                                                                )
                                                          )
                                                        ]
                                                    , expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "port declaration for command" <|
            \() ->
                "port parseResponse : ( String, String ) -> Cmd msg"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 51 } }
                            (PortDeclaration
                                { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 19 } } "parseResponse"
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 22 }, end = { row = 1, column = 51 } }
                                        (FunctionTypeAnnotation
                                            (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 40 } }
                                                (Tupled
                                                    [ Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } (Typed (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                    , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } }
                                                        (Typed
                                                            (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } } ( [], "String" ))
                                                            []
                                                        )
                                                    ]
                                                )
                                            )
                                            (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 51 } }
                                                (Typed
                                                    (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } ( [], "Cmd" ))
                                                    [ Node { start = { row = 1, column = 48 }, end = { row = 1, column = 51 } } (GenericType "msg") ]
                                                )
                                            )
                                        )
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                "port scroll : (Move -> msg) -> Sub msg"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (PortDeclaration
                                { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } "scroll"
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 15 }, end = { row = 1, column = 39 } }
                                        (FunctionTypeAnnotation
                                            (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 28 } }
                                                (FunctionTypeAnnotation
                                                    (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                        (Typed (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } ( [], "Move" )) [])
                                                    )
                                                    (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (GenericType "msg"))
                                                )
                                            )
                                            (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 39 } }
                                                (Typed (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 35 } } ( [], "Sub" ))
                                                    [ Node { start = { row = 1, column = 36 }, end = { row = 1, column = 39 } } (GenericType "msg")
                                                    ]
                                                )
                                            )
                                        )
                                }
                            )
                        )
        , test "should fail to parse destructuring declaration at the top-level" <|
            \() ->
                "_ = b"
                    |> expectInvalid
        , test "declaration" <|
            \() ->
                """main =
  text "Hello, World!\""""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text")
                                                    , Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Literal "Hello, World!")
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function" <|
            \() ->
                """main =
  text "Hello, World!\""""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text")
                                                    , Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (Literal "Hello, World!")
                                                    ]
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "function starting with multi line comment" <|
            \() ->
                """main =
  {- y -} x"""
                    |> expectAstWithComments
                        { ast =
                            Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                (FunctionDeclaration
                                    { documentation = Nothing
                                    , signature = Nothing
                                    , declaration =
                                        Node { start = { row = 1, column = 1 }, end = { row = 2, column = 12 } }
                                            { arguments = []
                                            , expression = Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (FunctionOrValue [] "x")
                                            , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            }
                                    }
                                )
                        , comments = [ Node { start = { row = 2, column = 3 }, end = { row = 2, column = 10 } } "{- y -}" ]
                        }
        , test "function with a lot of symbols" <|
            \() ->
                "updateState update sendPort = curry <| (uncurry update) >> batchStateCmds sendPort"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                        { arguments = [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (VarPattern "update"), Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (VarPattern "sendPort") ]
                                        , expression =
                                            Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                (OperatorApplication "<|"
                                                    Right
                                                    (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "curry"))
                                                    (Node { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                        (OperatorApplication ">>"
                                                            Right
                                                            (Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                (ParenthesizedExpression
                                                                    (Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                        (Application
                                                                            [ Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (FunctionOrValue [] "uncurry")
                                                                            , Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (FunctionOrValue [] "update")
                                                                            ]
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            (Node { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                (Application
                                                                    [ Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (FunctionOrValue [] "batchStateCmds")
                                                                    , Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (FunctionOrValue [] "sendPort")
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "Some function" <|
            \() ->
                """update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (CaseExpression
                                                    { cases =
                                                        [ ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                                          , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                                (OperatorApplication "+"
                                                                    Left
                                                                    (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (Integer 1))
                                                                )
                                                          )
                                                        , ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (OperatorApplication "-"
                                                                    Left
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (Integer 1))
                                                                )
                                                          )
                                                        ]
                                                    , expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    }
                                                )
                                        , name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature = Nothing
                                }
                            )
                        )
        , test "some other function" <|
            \() ->
                """update : Model
update msg model =
    msg"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 3, column = 8 } }
                            (FunctionDeclaration
                                { declaration =
                                    Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                        { arguments =
                                            [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (VarPattern "msg")
                                            , Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (VarPattern "model")
                                            ]
                                        , expression = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (FunctionOrValue [] "msg")
                                        , name = Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                        }
                                , documentation = Nothing
                                , signature =
                                    Just
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , typeAnnotation = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Typed (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                            }
                                        )
                                }
                            )
                        )
        , test "regression test for disallowing ( +)" <|
            \() ->
                "a = ( +)"
                    |> expectInvalid
        , test "regression test for disallowing (+ )" <|
            \() ->
                "a = (+ )"
                    |> expectInvalid
        ]


expectAst : Node Declaration -> String -> Expect.Expectation
expectAst =
    CombineTestUtil.expectAst declaration


expectAstWithComments : { ast : Node Declaration, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    CombineTestUtil.expectAstWithComments declaration


expectInvalid : String -> Expect.Expectation
expectInvalid =
    CombineTestUtil.expectInvalid declaration
