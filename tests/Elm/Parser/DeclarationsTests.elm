module Elm.Parser.DeclarationsTests exposing (all)

import Elm.Parser.Declarations exposing (..)
import Elm.Parser.ParserWithCommentsTestUtil as ParserWithCommentsUtil exposing (parse)
import Elm.Syntax.Declaration as Declaration exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression as Expression exposing (..)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (..)
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "bar")
                                        }
                                }
                            )
                        )
        , test "function declaration with documentation" <|
            \() ->
                """{-| Foo does bar -}
foo = bar"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 10 } }
                            (FunctionDeclaration
                                { documentation = Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 20 } } "{-| Foo does bar -}")
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 2, column = 1 }, end = { row = 2, column = 10 } }
                                        { name = Node { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Node { start = { row = 2, column = 7 }, end = { row = 2, column = 10 } } (FunctionOrValue [] "bar")
                                        }
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
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression = Node { start = { row = 1, column = 7 }, end = { row = 1, column = 9 } } (Expression.Record [])
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
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern_ "x") ]
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
                                                                                (Case
                                                                                    { expression = Node { start = { row = 4, column = 12 }, end = { row = 4, column = 13 } } (FunctionOrValue [] "x")
                                                                                    , firstCase =
                                                                                        ( Node { start = { row = 5, column = 9 }, end = { row = 5, column = 13 } } (NamedPattern { moduleName = [], name = "True" } [])
                                                                                        , Node { start = { row = 5, column = 17 }, end = { row = 5, column = 18 } } (FunctionOrValue [] "z")
                                                                                        )
                                                                                    , restOfCases = []
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
                                        , arguments = [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 6 } } (VarPattern_ "x") ]
                                        , expression =
                                            Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } }
                                                (Operation "+"
                                                    Infix.Left
                                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (FunctionOrValue [] "x"))
                                                    (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 14 } } (IntegerLiteral 1))
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                            (LetFunction
                                                                { documentation = Nothing
                                                                , signature = Nothing
                                                                , declaration =
                                                                    Node { start = { row = 3, column = 3 }, end = { row = 3, column = 8 } }
                                                                        { name = Node { start = { row = 3, column = 3 }, end = { row = 3, column = 4 } } "b"
                                                                        , arguments = []
                                                                        , expression = Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (IntegerLiteral 1)
                                                                        }
                                                                }
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        }
                                }
                            )
                        )
        , test "documentation comment inside a let is invalid" <|
            \() ->
                expectInvalid """foo =
 let
  {-| b is one -}
  b = 1
 in
  b"""
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 5, column = 4 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } "foo"
                                        , arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 2 }, end = { row = 5, column = 4 } }
                                                (LetExpression
                                                    { declarations =
                                                        [ Node { start = { row = 3, column = 3 }, end = { row = 3, column = 16 } }
                                                            (LetDestructuring
                                                                (Node { start = { row = 3, column = 3 }, end = { row = 3, column = 9 } }
                                                                    (TuplePattern_
                                                                        [ Node { start = { row = 3, column = 4 }, end = { row = 3, column = 5 } } (VarPattern_ "b")
                                                                        , Node { start = { row = 3, column = 7 }, end = { row = 3, column = 8 } } (VarPattern_ "c")
                                                                        ]
                                                                    )
                                                                )
                                                                (Node { start = { row = 3, column = 10 }, end = { row = 3, column = 16 } }
                                                                    (TupleExpression
                                                                        [ Node { start = { row = 3, column = 11 }, end = { row = 3, column = 12 } } (IntegerLiteral 1)
                                                                        , Node { start = { row = 3, column = 14 }, end = { row = 3, column = 15 } } (IntegerLiteral 2)
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        ]
                                                    , expression = Node { start = { row = 5, column = 3 }, end = { row = 5, column = 4 } } (FunctionOrValue [] "b")
                                                    }
                                                )
                                        }
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 62 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 62 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 18 } } (FunctionOrValue [] "beginnerProgram"))
                                                    [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 62 } }
                                                        (Expression.Record
                                                            [ Node { start = { row = 2, column = 21 }, end = { row = 2, column = 30 } }
                                                                ( Node { start = { row = 2, column = 21 }, end = { row = 2, column = 26 } } "model"
                                                                , Node { start = { row = 2, column = 29 }, end = { row = 2, column = 30 } } (IntegerLiteral 0)
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
                                        }
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern_ "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Case
                                                    { expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    , firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } } (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (Operation "+"
                                                                Left
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                (Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (IntegerLiteral 1))
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } } (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Operation "-"
                                                                    Left
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (IntegerLiteral 1))
                                                                )
                                                          )
                                                        ]
                                                    }
                                                )
                                        }
                                }
                            )
                        )
        , test "port declaration for command" <|
            \() ->
                "port parseResponse : ( String, String ) -> Cmd msg"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 51 } }
                            (PortDeclaration
                                { documentation = Nothing
                                , signature =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 51 } } <|
                                        { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 19 } } "parseResponse"
                                        , typeAnnotation =
                                            Node { start = { row = 1, column = 22 }, end = { row = 1, column = 51 } }
                                                (FunctionTypeAnnotation
                                                    (Node { start = { row = 1, column = 22 }, end = { row = 1, column = 40 } }
                                                        (Tuple
                                                            [ Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } (Type (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                            , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } }
                                                                (Type
                                                                    (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 38 } } ( [], "String" ))
                                                                    []
                                                                )
                                                            ]
                                                        )
                                                    )
                                                    (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 51 } }
                                                        (Type
                                                            (Node { start = { row = 1, column = 44 }, end = { row = 1, column = 47 } } ( [], "Cmd" ))
                                                            [ Node { start = { row = 1, column = 48 }, end = { row = 1, column = 51 } } (Var "msg") ]
                                                        )
                                                    )
                                                )
                                        }
                                }
                            )
                        )
        , test "port declaration for subscription" <|
            \() ->
                "port scroll : (Move -> msg) -> Sub msg"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (PortDeclaration
                                { documentation = Nothing
                                , signature =
                                    Node { start = { row = 1, column = 6 }, end = { row = 1, column = 39 } } <|
                                        { name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } "scroll"
                                        , typeAnnotation =
                                            Node { start = { row = 1, column = 15 }, end = { row = 1, column = 39 } }
                                                (FunctionTypeAnnotation
                                                    (Node { start = { row = 1, column = 15 }, end = { row = 1, column = 28 } }
                                                        (FunctionTypeAnnotation
                                                            (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } }
                                                                (Type (Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } ( [], "Move" )) [])
                                                            )
                                                            (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (Var "msg"))
                                                        )
                                                    )
                                                    (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 39 } }
                                                        (Type (Node { start = { row = 1, column = 32 }, end = { row = 1, column = 35 } } ( [], "Sub" ))
                                                            [ Node { start = { row = 1, column = 36 }, end = { row = 1, column = 39 } } (Var "msg")
                                                            ]
                                                        )
                                                    )
                                                )
                                        }
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (StringLiteral SingleQuote "Hello, World!")
                                                    ]
                                                )
                                        }
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 2, column = 23 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                        , arguments = []
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 2, column = 23 } }
                                                (Application
                                                    (Node { start = { row = 2, column = 3 }, end = { row = 2, column = 7 } } (FunctionOrValue [] "text"))
                                                    [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 23 } } (StringLiteral SingleQuote "Hello, World!")
                                                    ]
                                                )
                                        }
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
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } "main"
                                            , arguments = []
                                            , expression = Node { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } } (FunctionOrValue [] "x")
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 1, column = 83 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } "updateState"
                                        , arguments = [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 19 } } (VarPattern_ "update"), Node { start = { row = 1, column = 20 }, end = { row = 1, column = 28 } } (VarPattern_ "sendPort") ]
                                        , expression =
                                            Node { start = { row = 1, column = 31 }, end = { row = 1, column = 83 } }
                                                (Operation "<|"
                                                    Right
                                                    (Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "curry"))
                                                    (Node { start = { row = 1, column = 40 }, end = { row = 1, column = 83 } }
                                                        (Operation ">>"
                                                            Right
                                                            (Node { start = { row = 1, column = 40 }, end = { row = 1, column = 56 } }
                                                                (TupleExpression
                                                                    [ Node { start = { row = 1, column = 41 }, end = { row = 1, column = 55 } }
                                                                        (Application
                                                                            (Node { start = { row = 1, column = 41 }, end = { row = 1, column = 48 } } (FunctionOrValue [] "uncurry"))
                                                                            [ Node { start = { row = 1, column = 49 }, end = { row = 1, column = 55 } } (FunctionOrValue [] "update")
                                                                            ]
                                                                        )
                                                                    ]
                                                                )
                                                            )
                                                            (Node { start = { row = 1, column = 60 }, end = { row = 1, column = 83 } }
                                                                (Application (Node { start = { row = 1, column = 60 }, end = { row = 1, column = 74 } } (FunctionOrValue [] "batchStateCmds"))
                                                                    [ Node { start = { row = 1, column = 75 }, end = { row = 1, column = 83 } } (FunctionOrValue [] "sendPort")
                                                                    ]
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                        }
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
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = { row = 1, column = 1 }, end = { row = 7, column = 16 } }
                                        { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 8 }, end = { row = 1, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 1, column = 12 }, end = { row = 1, column = 17 } } (VarPattern_ "model")
                                            ]
                                        , expression =
                                            Node { start = { row = 2, column = 3 }, end = { row = 7, column = 16 } }
                                                (Case
                                                    { expression = Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (FunctionOrValue [] "msg")
                                                    , firstCase =
                                                        ( Node { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                                                            (NamedPattern { moduleName = [], name = "Increment" } [])
                                                        , Node { start = { row = 4, column = 7 }, end = { row = 4, column = 16 } }
                                                            (Operation "+"
                                                                Left
                                                                (Node { start = { row = 4, column = 7 }, end = { row = 4, column = 12 } } (FunctionOrValue [] "model"))
                                                                (Node { start = { row = 4, column = 15 }, end = { row = 4, column = 16 } } (IntegerLiteral 1))
                                                            )
                                                        )
                                                    , restOfCases =
                                                        [ ( Node { start = { row = 6, column = 5 }, end = { row = 6, column = 14 } }
                                                                (NamedPattern { moduleName = [], name = "Decrement" } [])
                                                          , Node { start = { row = 7, column = 7 }, end = { row = 7, column = 16 } }
                                                                (Operation "-"
                                                                    Left
                                                                    (Node { start = { row = 7, column = 7 }, end = { row = 7, column = 12 } } (FunctionOrValue [] "model"))
                                                                    (Node { start = { row = 7, column = 15 }, end = { row = 7, column = 16 } } (IntegerLiteral 1))
                                                                )
                                                          )
                                                        ]
                                                    }
                                                )
                                        }
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
                                { documentation = Nothing
                                , signature =
                                    Just
                                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                            { name = Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } } "update"
                                            , typeAnnotation = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (Type (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } ( [], "Model" )) [])
                                            }
                                        )
                                , declaration =
                                    Node { start = { row = 2, column = 1 }, end = { row = 3, column = 8 } }
                                        { name = Node { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } } "update"
                                        , arguments =
                                            [ Node { start = { row = 2, column = 8 }, end = { row = 2, column = 11 } } (VarPattern_ "msg")
                                            , Node { start = { row = 2, column = 12 }, end = { row = 2, column = 17 } } (VarPattern_ "model")
                                            ]
                                        , expression = Node { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } } (FunctionOrValue [] "msg")
                                        }
                                }
                            )
                        )
        , test "type alias" <|
            \() ->
                "type alias Foo = {color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 34 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 18 }, end = { row = 1, column = 34 } }
                                        (TypeAnnotation.Record
                                            [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 32 } }
                                                ( Node { start = { row = 1, column = 19 }, end = { row = 1, column = 24 } } "color"
                                                , Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } }
                                                    (Type (Node { start = { row = 1, column = 26 }, end = { row = 1, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias with documentation" <|
            \() ->
                """{-| Foo is colorful -}
type alias Foo = {color: String }"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 34 } }
                            (AliasDeclaration
                                { documentation = Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 23 } } "{-| Foo is colorful -}")
                                , name = Node { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Node { start = { row = 2, column = 18 }, end = { row = 2, column = 34 } }
                                        (TypeAnnotation.Record
                                            [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 32 } }
                                                ( Node { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } } "color"
                                                , Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } }
                                                    (Type (Node { start = { row = 2, column = 26 }, end = { row = 2, column = 32 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias without spacings around '='" <|
            \() ->
                "type alias Foo={color: String }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = []
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 16 }, end = { row = 1, column = 32 } }
                                        (TypeAnnotation.Record
                                            [ Node { start = { row = 1, column = 17 }, end = { row = 1, column = 30 } }
                                                ( Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } "color"
                                                , Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } }
                                                    (Type (Node { start = { row = 1, column = 24 }, end = { row = 1, column = 30 } } ( [], "String" )) [])
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type alias with Var " <|
            \() ->
                "type alias Foo a = {some : a }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 31 } }
                            (AliasDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 12 }, end = { row = 1, column = 15 } } "Foo"
                                , generics = [ Node { start = { row = 1, column = 16 }, end = { row = 1, column = 17 } } "a" ]
                                , typeAnnotation =
                                    Node { start = { row = 1, column = 20 }, end = { row = 1, column = 31 } }
                                        (TypeAnnotation.Record
                                            [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 29 } }
                                                ( Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } "some"
                                                , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 29 } } (Var "a")
                                                )
                                            ]
                                        )
                                }
                            )
                        )
        , test "type" <|
            \() ->
                "type Color = Blue String | Red | Green"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 39 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Color"
                                , generics = []
                                , firstConstructor =
                                    Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                        { name = Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "Blue"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } }
                                                (Type (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 25 } } ( [], "String" )) [])
                                            ]
                                        }
                                , restOfConstructors =
                                    [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } }
                                        { name = Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } "Red"
                                        , arguments = []
                                        }
                                    , Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } }
                                        { name = Node { start = { row = 1, column = 34 }, end = { row = 1, column = 39 } } "Green"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
        , test "type with documentation" <|
            \() ->
                """{-| Classic RGB -}
type Color = Blue String | Red | Green"""
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 39 } }
                            (CustomTypeDeclaration
                                { documentation = Just (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 19 } } "{-| Classic RGB -}")
                                , name = Node { start = { row = 2, column = 6 }, end = { row = 2, column = 11 } } "Color"
                                , generics = []
                                , firstConstructor =
                                    Node
                                        { start = { row = 2, column = 14 }
                                        , end = { row = 2, column = 25 }
                                        }
                                        { name = Node { start = { row = 2, column = 14 }, end = { row = 2, column = 18 } } "Blue"
                                        , arguments =
                                            [ Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } }
                                                (Type (Node { start = { row = 2, column = 19 }, end = { row = 2, column = 25 } } ( [], "String" )) [])
                                            ]
                                        }
                                , restOfConstructors =
                                    [ Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } }
                                        { name = Node { start = { row = 2, column = 28 }, end = { row = 2, column = 31 } } "Red"
                                        , arguments = []
                                        }
                                    , Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } }
                                        { name = Node { start = { row = 2, column = 34 }, end = { row = 2, column = 39 } } "Green"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
        , test "type with multiple args" <|
            \() ->
                "type D = C a B"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , firstConstructor =
                                    Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } (Var "a")
                                            , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } }
                                                (Type (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } ( [], "B" )) [])
                                            ]
                                        }
                                , restOfConstructors = []
                                }
                            )
                        )
        , test "type with multiple args and correct distribution of args" <|
            \() ->
                "type D = C B a"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } "D"
                                , generics = []
                                , firstConstructor =
                                    Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } }
                                        { name = Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } "C"
                                        , arguments =
                                            [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } }
                                                (Type (Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } ( [], "B" )) [])
                                            , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (Var "a")
                                            ]
                                        }
                                , restOfConstructors = []
                                }
                            )
                        )
        , test "type args should not continue on next line" <|
            \() ->
                "type D = C B\na"
                    |> expectInvalid
        , test "type with Var" <|
            \() ->
                "type Maybe a = Just a | Nothing"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 32 } }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = Node { start = { row = 1, column = 6 }, end = { row = 1, column = 11 } } "Maybe"
                                , generics = [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 13 } } "a" ]
                                , firstConstructor =
                                    Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } }
                                        { name = Node { start = { row = 1, column = 16 }, end = { row = 1, column = 20 } } "Just"
                                        , arguments = [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 22 } } (Var "a") ]
                                        }
                                , restOfConstructors =
                                    [ Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } }
                                        { name = Node { start = { row = 1, column = 25 }, end = { row = 1, column = 32 } } "Nothing"
                                        , arguments = []
                                        }
                                    ]
                                }
                            )
                        )
        , test "type with value on next line " <|
            \() ->
                parse "type Maybe a = Just a |\nNothing" Elm.Parser.Declarations.declaration
                    |> Expect.equal Nothing
        , test "fail if declarations not on module-level" <|
            \() ->
                """a = f
    3
    b = 4"""
                    |> expectInvalid
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
    ParserWithCommentsUtil.expectAst declaration


expectAstWithComments : { ast : Node Declaration, comments : List (Node String) } -> String -> Expect.Expectation
expectAstWithComments =
    ParserWithCommentsUtil.expectAstWithComments declaration


expectInvalid : String -> Expect.Expectation
expectInvalid =
    ParserWithCommentsUtil.expectInvalid declaration
